#Requires -Version 5.1
<#
.SYNOPSIS
  Windows controller for the private GCE solo098 workstation.
.DESCRIPTION
  One-click: if RUNNING, connect immediately; if TERMINATED, show a GUI prompt
  that accepts after five seconds unless cancelled. Shutdown from the guest
  uses systemctl poweroff. -Desligar is an emergency external fallback only.
  Closing RDP does NOT stop the VM.
  No public VM address, Cloudflare or saved password is used.
.NOTES
  Requires a locally authenticated gcloud.cmd, appropriate GCE/IAP IAM
  permissions, and mstsc.exe. Review this file before first execution.
#>
[CmdletBinding()]
param(
  [switch] $Conectar,
  [switch] $Desligar,
  [switch] $InstalarAtalho
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$script:Project = 'argon-surf-472319-b0'
$script:Zone = 'southamerica-east1-b'
$script:Region = 'southamerica-east1'
$script:Vm = 'solo098'
$script:Router = 'solo098-router'
$script:Nat = 'solo098-nat'
$script:Subnet = 'solo098-subnet'
$script:LocalPort = 13389

if (([int][bool]$Conectar + [int][bool]$Desligar + [int][bool]$InstalarAtalho) -gt 1) {
  throw 'Use apenas uma opcao: -Conectar, -Desligar ou -InstalarAtalho.'
}

function Install-Shortcut {
  if (-not $PSCommandPath) { throw 'Salve o script em um arquivo .ps1 primeiro.' }
  $desktop = [Environment]::GetFolderPath('Desktop')
  $linkPath = Join-Path $desktop 'Workstation GCE.lnk'
  $wsh = New-Object -ComObject WScript.Shell
  $link = $wsh.CreateShortcut($linkPath)
  $link.TargetPath = (Join-Path $PSHOME 'powershell.exe')
  $link.Arguments = '-NoProfile -File "' + $PSCommandPath + '"'
  $link.WorkingDirectory = Split-Path -Parent $PSCommandPath
  $link.Description = 'Conectar ou desligar a workstation solo098 via Google Cloud IAP'
  $link.Save()
  Write-Host "Atalho criado: $linkPath"
}

if ($InstalarAtalho) {
  Install-Shortcut
  return
}

$cmd = Get-Command 'gcloud.cmd' -ErrorAction SilentlyContinue
if (-not $cmd) {
  throw 'gcloud.cmd nao encontrado no PATH. Instale e autentique o Google Cloud CLI no Windows.'
}
$script:Gcloud = $cmd.Source

function Invoke-Gcloud {
  param([Parameter(Mandatory=$true)][string[]] $CommandArguments)
  $output = & $script:Gcloud @CommandArguments 2>&1
  $exitCode = $LASTEXITCODE
  if ($exitCode -ne 0) {
    throw ('gcloud falhou (codigo {0}): {1}' -f $exitCode, (($output | Out-String).Trim()))
  }
  return $output
}

function Get-VmStatus {
  $output = Invoke-Gcloud -CommandArguments @(
    'compute', 'instances', 'describe', $script:Vm,
    "--project=$script:Project", "--zone=$script:Zone", '--format=value(status)'
  )
  $state = (($output | Out-String).Trim())
  if ($state -notin @('RUNNING', 'TERMINATED')) {
    throw "Estado da VM nao pronto para operacao: [$state]. Nao alterei os recursos."
  }
  return $state
}

function Test-NatExists {
  $names = @(Invoke-Gcloud -CommandArguments @(
    'compute', 'routers', 'nats', 'list',
    "--project=$script:Project", "--region=$script:Region",
    "--router=$script:Router", '--format=value(name)'
  ))
  foreach ($name in $names) {
    if (([string]$name).Trim() -eq $script:Nat) { return $true }
  }
  return $false
}

function Ensure-Nat {
  if (Test-NatExists) {
    Write-Host "Cloud NAT $script:Nat ja existe."
    return
  }
  Write-Host "Criando Cloud NAT $script:Nat (gera custo enquanto ativo)..."
  Invoke-Gcloud -CommandArguments @(
    'compute', 'routers', 'nats', 'create', $script:Nat,
    "--project=$script:Project", "--region=$script:Region",
    "--router=$script:Router", '--auto-allocate-nat-external-ips',
    "--nat-custom-subnet-ip-ranges=$script:Subnet"
  ) | Out-Host
  if (-not (Test-NatExists)) { throw 'Cloud NAT nao apareceu apos a criacao.' }
}

function Ensure-VmRunning {
  $state = Get-VmStatus
  Ensure-Nat
  if ($state -eq 'TERMINATED') {
    Write-Host "Iniciando VM $script:Vm (gera custo por hora de execucao)..."
    Invoke-Gcloud -CommandArguments @(
      'compute', 'instances', 'start', $script:Vm,
      "--project=$script:Project", "--zone=$script:Zone"
    ) | Out-Host
  }
  if ((Get-VmStatus) -ne 'RUNNING') { throw 'VM nao ficou em RUNNING.' }
}

function Stop-VmAndNat {
  $state = Get-VmStatus
  if ($state -eq 'RUNNING') {
    Write-Host 'Parando a VM (o disco persistente continua sendo cobrado)...'
    Invoke-Gcloud -CommandArguments @(
      'compute', 'instances', 'stop', $script:Vm,
      "--project=$script:Project", "--zone=$script:Zone"
    ) | Out-Host
  }
  if ((Get-VmStatus) -ne 'TERMINATED') {
    throw 'Nao removi o NAT: a VM ainda nao esta TERMINATED.'
  }
  if (Test-NatExists) {
    Write-Host 'Excluindo o Cloud NAT para evitar cobranca quando a VM esta desligada...'
    Invoke-Gcloud -CommandArguments @(
      'compute', 'routers', 'nats', 'delete', $script:Nat,
      "--project=$script:Project", "--region=$script:Region",
      "--router=$script:Router", '--quiet'
    ) | Out-Host
  }
  if (Test-NatExists) { throw 'O Cloud NAT ainda existe; revise custos no Google Cloud.' }
  Write-Host 'VM TERMINATED e NAT ausente. Disco, imagem e outros recursos persistem.'
}

function Test-LocalPort {
  $client = New-Object System.Net.Sockets.TcpClient
  try {
    $iar = $client.BeginConnect('127.0.0.1', $script:LocalPort, $null, $null)
    if (-not $iar.AsyncWaitHandle.WaitOne(300)) { return $false }
    $client.EndConnect($iar)
    return $client.Connected
  }
  catch {
    return $false
  }
  finally {
    $client.Dispose()
  }
}

function Connect-Rdp {
  Ensure-VmRunning
  if (Test-LocalPort) {
    throw "A porta local $script:LocalPort ja esta em uso; nao conectei para evitar abrir uma sessao errada."
  }
  Write-Host "Abrindo tunel IAP TCP 3389 -> localhost:$script:LocalPort..."
  $args = @(
    'compute', 'start-iap-tunnel', $script:Vm, '3389',
    "--project=$script:Project", "--zone=$script:Zone",
    "--local-host-port=localhost:$script:LocalPort"
  )
  $tunnel = Start-Process -FilePath $script:Gcloud -ArgumentList $args -PassThru -WindowStyle Minimized
  try {
    $ready = $false
    for ($i = 0; $i -lt 45; $i++) {
      if ($tunnel.HasExited) { throw 'O processo do tunel IAP terminou antes de abrir a porta.' }
      if (Test-LocalPort) { $ready = $true; break }
      Start-Sleep -Milliseconds 700
    }
    if (-not $ready) { throw 'O tunel IAP nao abriu localhost:13389. Verifique IAM, gcloud e XRDP.' }
    Write-Host 'Abrindo MSTSC. Fechar o RDP nao desliga a VM.'
    Start-Process -FilePath 'mstsc.exe' -ArgumentList @(
      "/v:127.0.0.1:$script:LocalPort", '/f'
    ) -Wait
  }
  finally {
    # taskkill /T terminates the cmd wrapper and any gcloud child it spawned.
    if ($tunnel -and -not $tunnel.HasExited) {
      & taskkill.exe /PID $tunnel.Id /T /F 2>$null | Out-Null
    }
  }
  Write-Host 'Sessao RDP encerrada. A VM permanece RUNNING e faturando. Escolha D para desligar.'
}

# Legacy emergency stop remains available as -Desligar. The normal UX never
# attempts to delete Cloud NAT from a guest shutdown hook. Use an independent
# GCP reconciler for that; see RUNBOOK.md.
if ($Desligar) {
  Stop-VmAndNat
  return
}

function Confirm-StartWithCountdown {
  Add-Type -AssemblyName System.Windows.Forms
  Add-Type -AssemblyName System.Drawing
  $form = New-Object System.Windows.Forms.Form
  $form.Text = 'Workstation GCE - solo098'
  $form.Width = 440
  $form.Height = 180
  $form.StartPosition = 'CenterScreen'
  $form.FormBorderStyle = 'FixedDialog'
  $form.MaximizeBox = $false
  $form.MinimizeBox = $false
  $form.Topmost = $true

  $label = New-Object System.Windows.Forms.Label
  $label.AutoSize = $false
  $label.SetBounds(18, 16, 390, 52)
  $form.Controls.Add($label)

  $start = New-Object System.Windows.Forms.Button
  $start.Text = 'Ligar e conectar'
  $start.SetBounds(85, 86, 145, 30)
  $start.DialogResult = [System.Windows.Forms.DialogResult]::Yes
  $form.Controls.Add($start)

  $cancel = New-Object System.Windows.Forms.Button
  $cancel.Text = 'Cancelar'
  $cancel.SetBounds(242, 86, 100, 30)
  $cancel.DialogResult = [System.Windows.Forms.DialogResult]::Cancel
  $form.Controls.Add($cancel)
  $form.AcceptButton = $start
  $form.CancelButton = $cancel

  $remaining = 5
  $timer = New-Object System.Windows.Forms.Timer
  $timer.Interval = 1000
  $timer.Add_Tick({
    $remaining--
    if ($remaining -le 0) {
      $timer.Stop()
      $form.DialogResult = [System.Windows.Forms.DialogResult]::Yes
      $form.Close()
    }
    else {
      $label.Text = "A VM solo098 esta desligada. Ligar e conectar automaticamente em $remaining segundos?"
    }
  })
  $label.Text = "A VM solo098 esta desligada. Ligar e conectar automaticamente em $remaining segundos?"
  $form.Add_Shown({ $timer.Start() })
  try { return ($form.ShowDialog() -eq [System.Windows.Forms.DialogResult]::Yes) }
  finally { $timer.Stop(); $timer.Dispose(); $form.Dispose() }
}

# One-click: already RUNNING -> connect immediately; TERMINATED -> ask in GUI,
# auto-accept after five seconds unless cancelled. Unknown state -> fail closed.
$state = Get-VmStatus
if ($state -eq 'TERMINATED' -and -not (Confirm-StartWithCountdown)) {
  Write-Host 'Inicio cancelado; recursos inalterados.'
  return
}
try {
  Connect-Rdp
}
catch {
  Write-Warning $_.Exception.Message
  Write-Warning 'Confira a VM e Cloud NAT no GCP antes de tentar novamente.'
  throw
}
