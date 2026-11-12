# Runbook da workstation solo098

Estado documental: 12 de novembro de 2026. Este procedimento descreve **uma reconstrução futura** e as verificações reais feitas no primeiro deploy; não é prova de que todo o desktop esteja validado. Nunca execute o provisionamento duas vezes sem reconciliar os recursos pagos existentes.

## Escopo e fonte de verdade

- Repositório: klarkc/dotfiles, branch servidor-hj-mesmo. Os scripts de provisionamento, bootstrap, PGP e Windows são versionados nesta pasta.
- Projeto GCP: argon-surf-472319-b0; região southamerica-east1; zona southamerica-east1-b; VM solo098.
- Máquina e armazenamento: e2-highmem-4, 4 vCPU, 32 GiB RAM, 256 GB pd-standard; root Btrfs único, incluindo /home e /nix. Sem IP público.
- Rede: solo098-vpc, solo098-subnet (10.146.0.0/24), solo098-router, solo098-nat; ingress TCP 22 e 3389 somente pelo IAP (35.235.240.0/20).
- Acesso Windows: Google Cloud CLI autenticado + IAP TCP 3389 -> 127.0.0.1:13389 -> MSTSC; sem Cloudflare no RDP. ChatGPT/MCP usa caminho Cloudflare Access distinto.
- Objetivo mensal: teto R$560 all-in, referência de 176 horas ligadas; valores são estimativas, não há desligamento automático nem bloqueio de gastos. Disco, imagens e armazenamento persistem após parar VM. NAT deve ser removido ao desligar, para evitar cobranças de NAT/IP.
- Sistema de usuário: Arch Linux + Determinate Nix (não pacman/nix). Preservar XMonad, Taffybar, feh, PipeWire, picom xrender, keyd, screenshot tools, bloqueio manual, Haskell, gvim, git-lfs, Brave e Enpass X11; desabilitar somente serviços de GPU incompatíveis.

## 1. Pré-requisitos de Cloud Shell

1. Possuir direitos Compute Engine/IAP, Cloud Storage e Cloud Router no projeto. Revisar custo e saldo; confirmar região e quantos recursos já existem.
2. Cloud Shell é efêmero: pacotes apt não sobrevivem necessariamente à substituição da VM do Shell. Instalar qemu-img usando qemu-utils, se faltar:

```bash
sudo apt-get update
sudo apt-get install -y qemu-utils
qemu-img --version
```

3. Clonar a branch, sem executar scripts de um checkout sujo:

```bash
git clone --branch servidor-hj-mesmo https://github.com/klarkc/dotfiles.git ~/dotfiles
cd ~/dotfiles
git status --short
git rev-parse HEAD
```

4. Verificar o Test CI no **SHA exato**, sem aceitar o verde de um SHA anterior. O próprio script falha fechado.

```bash
bash .nix/servidor-hj-mesmo/preflight.sh
bash .nix/servidor-hj-mesmo/provision-gce.sh --verify-only
```

O verify-only baixa imagem QCOW2 oficial Arch 2026-11-01 assinada, confere PGP, SHA256 e qemu-img, converte em RAW sparse + oldgnu disk.tar.gz e não cria recursos faturáveis. O QCOW2 tem 576697856 bytes, virtual 2 GiB. O download pode ser retomado em /tmp/solo098-arch-cache-UID, **não** em /home; mudanças de sessão podem perder o cache.

## 2. Provisionar do zero (pago)

**Somente** com consentimento explícito para criar recursos, inventário conferido e CI verde:

```bash
cd ~/dotfiles
bash .nix/servidor-hj-mesmo/provision-gce.sh --apply
```

O script pede CRIAR_SOLO098_256 antes de qualquer escrita faturável; importa a imagem de um bucket staging, cria VPC/subnet/router/NAT/firewalls e a VM, preservando o disco quando parar ou excluir a VM. O script recusa uma VM de mesmo nome já existente. Não execute --apply novamente às cegas se falhar depois da criação do bucket. Liste o que existe, concilie custos e prossiga manualmente com acompanhamento.

Verificação de estado:

```bash
gcloud compute images describe solo098-arch-20261101 --project=argon-surf-472319-b0
gcloud compute instances describe solo098 --project=argon-surf-472319-b0 --zone=southamerica-east1-b --format='yaml(name,status,networkInterfaces,disks,metadata)'
gcloud compute routers nats list --router=solo098-router --region=southamerica-east1 --project=argon-surf-472319-b0
gcloud storage ls gs://argon-surf-472319-b0-solo098-image-staging
```

A importação gera uma imagem personalizada independente do tar no bucket. Após confirmar READY e o primeiro boot, avaliar a exclusão segura do objeto e bucket de staging para reduzir custo, sem remover a imagem de boot.

## 3. Primeiro boot, disco, metadata e SSH

Verificar serial, especialmente cloud-init:

```bash
gcloud compute instances get-serial-port-output solo098 \
  --project=argon-surf-472319-b0 --zone=southamerica-east1-b --port=1 | tail -n 80
```

O usuário klarkc é criado sem senha inicial, apenas com chave SSH pública, e a chave privada é criada no Cloud Shell em ~/.ssh/solo098-cloudshell. Verificar o fingerprint de host SSH contra a saída do console antes de aceitar a conexão. **Com --plain, passar a chave privada explicitamente**:

```bash
gcloud compute ssh klarkc@solo098 \
  --project=argon-surf-472319-b0 --zone=southamerica-east1-b \
  --tunnel-through-iap --plain --ssh-key-file="$HOME/.ssh/solo098-cloudshell" \
  -- -i "$HOME/.ssh/solo098-cloudshell" \
  -o IdentitiesOnly=yes -o PreferredAuthentications=publickey \
  -o PasswordAuthentication=no
```

Dentro da VM:

```bash
whoami
cat /etc/os-release
cloud-init status --long
lsblk -f
df -h /
getent ahostsv4 metadata.google.internal
curl --noproxy '*' -fsS -H 'Metadata-Flavor: Google' \
  http://metadata.google.internal/computeMetadata/v1/instance/attributes/solo098-dotfiles-sha
```

No primeiro deploy real (2026-11-12), o Arch inicializou, cloud-init reportou done com avisos DNS transitórios e /dev/sda3 Btrfs cresceu automaticamente de 2 GiB para aproximadamente 256 GiB. Nome e IP 169.254.169.254 do metadata retornaram o SHA pinado. Se o DNS falhar após boot, verificar systemd-resolved antes de modificar configuração.

Definir senha local interativa para XRDP e sudo; nunca em metadata, Git ou chat:

```bash
sudo passwd klarkc
```

## 4. Bootstrap guest e identidade do Nix

O script guest-bootstrap.sh instala pacotes de GUI, AUR (xrdp, xorgxrdp, brave-bin), Enpass X11 e **Determinate Nix v3.24.2** com checksum fixado; o script falha fechado para pacman/nix ou estado parcial de /nix. Ele exige correspondência entre **HEAD local** e metadata solo098-dotfiles-sha. Não habilitar CUDA/Sunshine ou remover funcionalidades.

Para a primeira VM sem checkout, baixar o bootstrap do SHA de metadata, verificar sintaxe e executar. O script cria /home/klarkc com o checkout autenticado, mas a branch pública deve continuar no mesmo commit até a clonagem. Preferir preparar uma tag/commit imutável e melhorar o clone pinado antes de qualquer reconstrução futura se o branch HEAD tiver avançado.

```bash
SHA="$(curl --noproxy '*' -fsS -H 'Metadata-Flavor: Google' \
  http://metadata.google.internal/computeMetadata/v1/instance/attributes/solo098-dotfiles-sha)"
curl -fLsS --retry 3 \
  "https://raw.githubusercontent.com/klarkc/dotfiles/$SHA/.nix/servidor-hj-mesmo/guest-bootstrap.sh" \
  -o /tmp/solo098-guest-bootstrap.sh
bash -n /tmp/solo098-guest-bootstrap.sh
set -o pipefail
sudo bash /tmp/solo098-guest-bootstrap.sh 2>&1 | tee -a "$HOME/solo098-guest-bootstrap.log"
```

Se o checkout já está em /home/klarkc, usar **seu script versionado** após conferir metadata e HEAD; não usar o /tmp desatualizado. Não alterar metadata enquanto o bootstrap está em execução.

### Instalação das configurações do usuário (Makefile compartilhado)

O `guest-bootstrap.sh` original executou `nix profile add .#solo098-gce`, mas **não executou `make`**. O Makefile aceita agora a flag `GCE=1` para selecionar o perfil **CPU-only** sem pular as demais tarefas de tema Nordic, ícones Papirus, Vim, tmux, Git e npm. Não executar `make` sem a flag nesta VM: o padrão continua usando o perfil com vLLM/CUDA do host original. Não rodar `make` como root, nem atualizar automaticamente o checkout/metadata pinado durante outros trabalhos.

Executar somente após CI verde no SHA escolhido, revisão do Makefile e atualização coordenada do checkout+metadata (não substituir o SHA da VM só porque a branch avançou):

```bash
cd /home/klarkc
make -n GCE=1
make GCE=1
systemctl --user daemon-reload
```

Para conferir só a seleção do perfil, sem executar instalação: `make -n GCE=1 nix.Profile`. A opção `GCE=0` (ou nenhuma flag) conserva a instalação histórica: `nix profile install .`. O modo `GCE=1` usa `nix profile add .#solo098-gce`; não cria outro Makefile e não muda os targets comuns. A instalação do perfil Nix já foi feita no bootstrap inicial, mas as tarefas de interface não foram. **O pós-bootstrap do primeiro deploy ainda não foi executado/validado**.

Alguns alvos originais precisam de dependências do usuário (como `npm`, `gsettings`, `xfconf-query`, `vim` e `tmux`) e baixam temas de URLs upstream sem pinning imutável; esse risco precisa de revisão antes de repetir em produção e os passos não devem rodar enquanto houver outras instalações. A ausência dessas dependências não autoriza remover funcionalidades do dotfiles, instalar CUDA ou trocar o tema. Se faltar algo, diagnosticar primeiro e corrigir os requisitos de host/Makefile em commit testado.

A política de idioma **não depende de GCE**. Tanto `make` quanto `make GCE=1` executam o alvo compartilhado `locale.Configure`, que chama `.nix/configure-locale-arch.sh` com sudo para gerar `en_US.UTF-8` e `pt_BR.UTF-8` e definir `pt_BR.UTF-8` como padrão do Arch. Em distribuições NixOS, gerar esses locales declarativamente em `i18n`, sem executar o helper Arch; esta documentação ainda cobre o host Arch temporário.

Na sessão gráfica, `.local/bin/xmonad-session` e `.config/environment.d/10-locale.conf` fornecem `LANG=pt_BR.UTF-8` e `LANGUAGE=pt_BR:pt` para aplicativos e serviços de usuário. O teclado XMonad é `br(abnt2)`, com CapsLock como Escape. No Bash interativo, `.bashrc` troca apenas para `LANG=en_US.UTF-8` e `LANGUAGE=en_US:en`, sem definir `LC_ALL`. Aplicativos gráficos abertos pelo terminal herdam inglês; executados pelo XMonad, usam português.

Após aplicar um SHA validado e executar `make GCE=1` **como usuário normal**, abra novas sessões SSH e XRDP. Use estas verificações somente de leitura:

```bash
locale -a
localectl status
locale
# Na sessão XMonad, consulte só as variáveis de idioma do processo:
pid="$(pgrep -u "$USER" -x xmonad | head -n 1)"
if [ -n "$pid" ]; then
  tr '\\0' '\\n' <"/proc/$pid/environ" | grep -E '^(LANG|LANGUAGE)='
fi
setxkbmap -query
```

O primeiro bootstrap da VM foi realizado antes do novo helper; o marcador `/var/lib/solo098-guest-installed` impede reexecutar todo o bootstrap. Aplique a configuração via `make GCE=1` após revisar o código e reconciliar o checkout e os metadados; não reexecute o bootstrap inteiro. A geração dos locales não resolve o OpenGL do XRDP: testar Mesa/llvmpipe antes de considerar patches de aplicativos. O login Google/Drive e a recuperação de `~/.profile_override` continuam sendo feitos pelo Enpass na GUI; não armazenar segredos no repositório.

### Lições do primeiro deploy em 2026-11-12

- CLI Cloud Storage: --public-access-prevention é flag booleana; não usar =enforced. Script atualizado usa --soft-delete-duration=0 no staging.
- Cloud Shell novo perdeu qemu-img e o cache QCOW2; apt install qemu-utils resolveu.
- O primeiro login SSH pediu senha porque --plain não repassava a chave. O login por -i, sem senha, funcionou.
- xorgxrdp 0.10.5 passou no SHA256, mas o makepkg não possuía a chave PGP 03993B4065E7193B. Importar sob klarkc (sem sudo), conferir fingerprint principal 61ECEABBF2BB40E3A35DF30A9F72CDBC01BF10EB e subchave 18AB838A907167745914871903993B4065E7193B; jamais usar --skippgpcheck. Hoje ainda há etapa manual e pin AUR dependente de revisão upstream.

```bash
gpg --keyserver hkps://keyserver.ubuntu.com \
  --recv-keys 61ECEABBF2BB40E3A35DF30A9F72CDBC01BF10EB
gpg --fingerprint --fingerprint 61ECEABBF2BB40E3A35DF30A9F72CDBC01BF10EB
```

- O primeiro bootstrap usou o pacote nix 2.35.6-1 do Arch e falhou porque /nix/store não existia. Foi substituído no commit CI-verde fa0fc9608ad6044149325841273762961831e189 por Determinate Nix. Durante a migração real, o operador removeu apenas pacman/nix, preservou /etc/nix sob /root/solo098-arch-nix-backup, confirmou que /nix continha somente diretórios e socket residuais (0 bytes), e identificou dez usuários nixbld01..10 e grupo nixbld GID 968. A limpeza foi controlada por testes de IDs/processos e remoção direcionada: nunca apagar /nix às cegas nem assumir que todas as instalações futuras terão o mesmo estado.
- O commit fa0fc960 usa Determinate Nix installer v3.24.2, SHA256 do binário x86_64-linux 22ed14ee1b24d100d388f1fc245e3062bd5982433d159cffa6dec34c0eb15dab. O bootstrap requer /nix/receipt.json, /nix/store e versão identificada como Determinate Nix, depois nix profile add .#solo098-gce.
- As observações acima são do histórico de depuração. **Uma instalação nova deve seguir o bootstrap corrente**, e não reproduzir a limpeza do Arch Nix se não houver resíduos.

## 5. Windows: GUI de conexão e desligamento seguro

Script canônico no repositório: .nix/servidor-hj-mesmo/solo098-control.ps1. Copiar esse arquivo revisado para o Windows; ele não será sincronizado automaticamente pelo checkout Linux. Requer gcloud.cmd no PATH (autenticado com conta e projeto corretos), IAM para ligar/parar Compute VM, gerenciar Cloud NAT e usar IAP; mstsc.exe presente.

No Windows PowerShell:

```powershell
gcloud.cmd auth login
gcloud.cmd config set project argon-surf-472319-b0
.\solo098-control.ps1 -InstalarAtalho
.\solo098-control.ps1
```

Fluxo atualizado: executar o script/atalho consulta o estado da VM. Se RUNNING, abre IAP/RDP diretamente. Se TERMINATED, mostra diálogo Windows Forms; **Ligar e conectar** é a resposta padrão após 5 segundos, mas Cancelar evita qualquer criação. O túnel abre somente em localhost:13389. Estados desconhecidos falham fechados. Sem Cloudflare no caminho RDP. A rotina -Desligar continua disponível como **fallback externo** e verifica TERMINATED antes de excluir NAT. Fechar o RDP **não** desliga a VM. O controlador GUI ainda precisa de E2E no Windows após a atualização.

Para permitir scripts no Windows PowerShell 5.1 (somente usuário): Set-ExecutionPolicy -Scope CurrentUser RemoteSigned; revisar o script pinado baixado do GitHub e executar Unblock-File exclusivamente nesse arquivo. Não usar Unrestricted.

### Desligamento a partir do Linux e garantia financeira

No desktop Linux, o operador pode executar `systemctl poweroff` após salvar seu trabalho. **Não afirmar que isso limpa NAT.** O desligamento iniciado pelo convidado deve ser acompanhado de leitura independente do estado GCE. Um hook do systemd pode não rodar em kernel panic, queda de energia, shutdown abrupto ou perda de rede e não é um lugar confiável para depender de credenciais amplas do GCP. A imagem Arch atual usa cloud-init e **não** instala o Google guest agent; o comportamento de transição para TERMINATED após poweroff ainda precisa de teste real. Se ficar RUNNING, usar `gcloud compute instances stop` no controlador externo após verificar ausência de tarefas importantes.

Procedimento de reconciliação externa (Cloud Shell ou Windows com gcloud autenticado; NÃO executar stop durante trabalho ativo):

```bash
gcloud compute instances describe solo098 --project=argon-surf-472319-b0 --zone=southamerica-east1-b --format='value(status)'
gcloud compute routers nats list --project=argon-surf-472319-b0 --region=southamerica-east1 --router=solo098-router
```

Se a VM estiver TERMINATED, executar `solo098-control.ps1 -Desligar` no Windows para remover o NAT. Esse fallback é idempotente na ausência de NAT. Em Cloud Shell, apenas depois de confirmar TERMINATED, pode ser usado:

```bash
gcloud compute routers nats delete solo098-nat --project=argon-surf-472319-b0 --region=southamerica-east1 --router=solo098-router --quiet
```

**Não existe garantia absoluta de custo zero após um poweroff.** O disco, imagens, staging, IPs e outros recursos podem persistir e continuar cobrados. Para tolerar queda do Windows ou do guest, considerar um reconciliador **externo** (Cloud Scheduler + Cloud Run/Function ou equivalente) que consulta o estado real e remove NAT **apenas se TERMINATED**, com IAM mínimo, operação idempotente, logs, retry, alerta e monitoramento de execução; também precisaríamos de política independente para VM presa em RUNNING e alertas de orçamento (alertas não são limites rígidos). Isso ainda **não foi implantado** e pode gerar despesas adicionais. Até aprovação explícita, realizar reconciliação manual.

### Diagnóstico Cloudflare / Desktop Commander autogerenciado

O perfil privado `/home/klarkc/.profile_override` é requisito do serviço `desktop-commander-mcp-tunnel.service`; o bootstrap termina mesmo sem enrollment inicial. A conexão Desktop Commander `solo098` foi vista **Offline em 12/11/2026**, enquanto GCE reportava RUNNING. Uma chamada ao conector autogerenciado falhou internamente: não prova que o cloudflared não está instalado, apenas que a cadeia ponta a ponta não está ativa.

Diagnóstico **somente leitura**, via SSH/IAP (não colar conteúdo do perfil privado nem tokens no chat):

```bash
test -s /home/klarkc/.profile_override && echo 'Perfil privado presente' || echo 'Perfil privado ausente/vazio'
ls -l /home/klarkc/.nix-profile/bin/desktop-commander-mcp-runtime
systemctl --user status desktop-commander-mcp-runtime.service desktop-commander-mcp-tunnel.service --no-pager
journalctl --user -u desktop-commander-mcp-runtime.service -u desktop-commander-mcp-tunnel.service -n 70 --no-pager
pgrep -af '[c]loudflared|[d]esktop-commander' || true
loginctl show-user klarkc -p Linger
```

Se o serviço de usuário não estiver disponível na sessão SSH, verificar `systemctl --user` com a sessão de usuário correta; não executar como root. Provisionamento Cloudflare/OAuth exige enrollment privado e consentimento antes de criar recursos. Segredos ficam apenas em `.profile_override`, nunca em Git, Cloud metadata, logs ou conversa. Os arquivos de serviço existem no dotfiles: `.config/systemd/user/desktop-commander-mcp-runtime.service` e `.config/systemd/user/desktop-commander-mcp-tunnel.service`. Para concluir, confirmar acesso via conector real, não apenas serviço ativo.

## 6. Testes de aceitação e encerramento

- Confirmar systemctl status xrdp xrdp-sesman (services active) e Determinate Nix/daemon funcional na VM.
- Via Windows, realizar RDP real, reconectar, validar XMonad/Taffybar, picom CPU-xrender, áudio posterior, Brave, Enpass X11 e ponte nativa para navegador.
- Testar Desktop Commander remoto via caminho Cloudflare Access/OAuth **separado do RDP**; em 12/11 mostrou Offline e enrollment ainda pendente. Manter credenciais somente em arquivo privado de usuário (nunca no Git).
- Confirmar GUI de 5 segundos, que VM já ligada conecta diretamente; validar botão Cancelar não cria recursos, reconexão, fallback -Desligar, que disco/imagem permanecem e que VM reinicia preservando dados. Validar poweroff do Linux, transição GCE para TERMINATED e remoção de NAT via reconciliador externo.
- VM desligada ainda gera custos de disco/imagem/staging; teto de R$560 não é enforcement automático. Verificar o painel de faturamento real e remover o staging após liberação.
- Evitar habilitar heartbeats/automação autônoma sem autorização explícita.

### Referências oficiais

- https://docs.cloud.google.com/iap/docs/using-tcp-forwarding
- https://docs.cloud.google.com/sdk/gcloud/reference/compute/routers/nats
- https://docs.cloud.google.com/sdk/gcloud/reference/compute/instances/add-metadata
- https://github.com/DeterminateSystems/nix-installer/releases/tag/v3.24.2

localectl status
locale | grep -E '^(LANG|LANGUAGE|LC_ALL)='
# Na sessão gráfica, verificar apenas os dois campos de idioma do XMonad:
pid="$(pgrep -u "$USER" -x xmonad | head -n 1)"
[ -z "$pid" ] || tr '\\0' '\\n' <"/proc/$pid/environ" | grep -E '^(LANG|LANGUAGE)='
setxkbmap -query
```

O primeiro bootstrap da VM em 12/11 ocorreu antes deste helper; o marcador `/var/lib/solo098-guest-installed` impede reexecutar o bootstrap completo. Aplicar a configuração pela flag `make GCE=1` após reconciliar o SHA do checkout/metadata, não por rebootstrap. A geração de locales não resolve OpenGL no XRDP: para Alacritty, testar Mesa/llvmpipe e não introduzir patches de aplicativos. O login Google/Drive e a recuperação de `~/.profile_override` continuam sendo feitos no Enpass pela GUI, sem publicar segredos.

### Lições do primeiro deploy em 2026-11-12

- CLI Cloud Storage: --public-access-prevention é flag booleana; não usar =enforced. Script atualizado usa --soft-delete-duration=0 no staging.
- Cloud Shell novo perdeu qemu-img e o cache QCOW2; apt install qemu-utils resolveu.
- O primeiro login SSH pediu senha porque --plain não repassava a chave. O login por -i, sem senha, funcionou.
- xorgxrdp 0.10.5 passou no SHA256, mas o makepkg não possuía a chave PGP 03993B4065E7193B. Importar sob klarkc (sem sudo), conferir fingerprint principal 61ECEABBF2BB40E3A35DF30A9F72CDBC01BF10EB e subchave 18AB838A907167745914871903993B4065E7193B; jamais usar --skippgpcheck. Hoje ainda há etapa manual e pin AUR dependente de revisão upstream.

```bash
gpg --keyserver hkps://keyserver.ubuntu.com \
  --recv-keys 61ECEABBF2BB40E3A35DF30A9F72CDBC01BF10EB
gpg --fingerprint --fingerprint 61ECEABBF2BB40E3A35DF30A9F72CDBC01BF10EB
```

- O primeiro bootstrap usou o pacote nix 2.35.6-1 do Arch e falhou porque /nix/store não existia. Foi substituído no commit CI-verde fa0fc9608ad6044149325841273762961831e189 por Determinate Nix. Durante a migração real, o operador removeu apenas pacman/nix, preservou /etc/nix sob /root/solo098-arch-nix-backup, confirmou que /nix continha somente diretórios e socket residuais (0 bytes), e identificou dez usuários nixbld01..10 e grupo nixbld GID 968. A limpeza foi controlada por testes de IDs/processos e remoção direcionada: nunca apagar /nix às cegas nem assumir que todas as instalações futuras terão o mesmo estado.
- O commit fa0fc960 usa Determinate Nix installer v3.24.2, SHA256 do binário x86_64-linux 22ed14ee1b24d100d388f1fc245e3062bd5982433d159cffa6dec34c0eb15dab. O bootstrap requer /nix/receipt.json, /nix/store e versão identificada como Determinate Nix, depois nix profile add .#solo098-gce.
- As observações acima são do histórico de depuração. **Uma instalação nova deve seguir o bootstrap corrente**, e não reproduzir a limpeza do Arch Nix se não houver resíduos.

## 5. Windows: GUI de conexão e desligamento seguro

Script canônico no repositório: .nix/servidor-hj-mesmo/solo098-control.ps1. Copiar esse arquivo revisado para o Windows; ele não será sincronizado automaticamente pelo checkout Linux. Requer gcloud.cmd no PATH (autenticado com conta e projeto corretos), IAM para ligar/parar Compute VM, gerenciar Cloud NAT e usar IAP; mstsc.exe presente.

No Windows PowerShell:

```powershell
gcloud.cmd auth login
gcloud.cmd config set project argon-surf-472319-b0
.\solo098-control.ps1 -InstalarAtalho
.\solo098-control.ps1
```

Fluxo atualizado: executar o script/atalho consulta o estado da VM. Se RUNNING, abre IAP/RDP diretamente. Se TERMINATED, mostra diálogo Windows Forms; **Ligar e conectar** é a resposta padrão após 5 segundos, mas Cancelar evita qualquer criação. O túnel abre somente em localhost:13389. Estados desconhecidos falham fechados. Sem Cloudflare no caminho RDP. A rotina -Desligar continua disponível como **fallback externo** e verifica TERMINATED antes de excluir NAT. Fechar o RDP **não** desliga a VM. O controlador GUI ainda precisa de E2E no Windows após a atualização.

Para permitir scripts no Windows PowerShell 5.1 (somente usuário): Set-ExecutionPolicy -Scope CurrentUser RemoteSigned; revisar o script pinado baixado do GitHub e executar Unblock-File exclusivamente nesse arquivo. Não usar Unrestricted.

### Desligamento a partir do Linux e garantia financeira

No desktop Linux, o operador pode executar `systemctl poweroff` após salvar seu trabalho. **Não afirmar que isso limpa NAT.** O desligamento iniciado pelo convidado deve ser acompanhado de leitura independente do estado GCE. Um hook do systemd pode não rodar em kernel panic, queda de energia, shutdown abrupto ou perda de rede e não é um lugar confiável para depender de credenciais amplas do GCP. A imagem Arch atual usa cloud-init e **não** instala o Google guest agent; o comportamento de transição para TERMINATED após poweroff ainda precisa de teste real. Se ficar RUNNING, usar `gcloud compute instances stop` no controlador externo após verificar ausência de tarefas importantes.

Procedimento de reconciliação externa (Cloud Shell ou Windows com gcloud autenticado; NÃO executar stop durante trabalho ativo):

```bash
gcloud compute instances describe solo098 --project=argon-surf-472319-b0 --zone=southamerica-east1-b --format='value(status)'
gcloud compute routers nats list --project=argon-surf-472319-b0 --region=southamerica-east1 --router=solo098-router
```

Se a VM estiver TERMINATED, executar `solo098-control.ps1 -Desligar` no Windows para remover o NAT. Esse fallback é idempotente na ausência de NAT. Em Cloud Shell, apenas depois de confirmar TERMINATED, pode ser usado:

```bash
gcloud compute routers nats delete solo098-nat --project=argon-surf-472319-b0 --region=southamerica-east1 --router=solo098-router --quiet
```

**Não existe garantia absoluta de custo zero após um poweroff.** O disco, imagens, staging, IPs e outros recursos podem persistir e continuar cobrados. Para tolerar queda do Windows ou do guest, considerar um reconciliador **externo** (Cloud Scheduler + Cloud Run/Function ou equivalente) que consulta o estado real e remove NAT **apenas se TERMINATED**, com IAM mínimo, operação idempotente, logs, retry, alerta e monitoramento de execução; também precisaríamos de política independente para VM presa em RUNNING e alertas de orçamento (alertas não são limites rígidos). Isso ainda **não foi implantado** e pode gerar despesas adicionais. Até aprovação explícita, realizar reconciliação manual.

### Diagnóstico Cloudflare / Desktop Commander autogerenciado

O perfil privado `/home/klarkc/.profile_override` é requisito do serviço `desktop-commander-mcp-tunnel.service`; o bootstrap termina mesmo sem enrollment inicial. A conexão Desktop Commander `solo098` foi vista **Offline em 12/11/2026**, enquanto GCE reportava RUNNING. Uma chamada ao conector autogerenciado falhou internamente: não prova que o cloudflared não está instalado, apenas que a cadeia ponta a ponta não está ativa.

Diagnóstico **somente leitura**, via SSH/IAP (não colar conteúdo do perfil privado nem tokens no chat):

```bash
test -s /home/klarkc/.profile_override && echo 'Perfil privado presente' || echo 'Perfil privado ausente/vazio'
ls -l /home/klarkc/.nix-profile/bin/desktop-commander-mcp-runtime
systemctl --user status desktop-commander-mcp-runtime.service desktop-commander-mcp-tunnel.service --no-pager
journalctl --user -u desktop-commander-mcp-runtime.service -u desktop-commander-mcp-tunnel.service -n 70 --no-pager
pgrep -af '[c]loudflared|[d]esktop-commander' || true
loginctl show-user klarkc -p Linger
```

Se o serviço de usuário não estiver disponível na sessão SSH, verificar `systemctl --user` com a sessão de usuário correta; não executar como root. Provisionamento Cloudflare/OAuth exige enrollment privado e consentimento antes de criar recursos. Segredos ficam apenas em `.profile_override`, nunca em Git, Cloud metadata, logs ou conversa. Os arquivos de serviço existem no dotfiles: `.config/systemd/user/desktop-commander-mcp-runtime.service` e `.config/systemd/user/desktop-commander-mcp-tunnel.service`. Para concluir, confirmar acesso via conector real, não apenas serviço ativo.

## 6. Testes de aceitação e encerramento

- Confirmar systemctl status xrdp xrdp-sesman (services active) e Determinate Nix/daemon funcional na VM.
- Via Windows, realizar RDP real, reconectar, validar XMonad/Taffybar, picom CPU-xrender, áudio posterior, Brave, Enpass X11 e ponte nativa para navegador.
- Testar Desktop Commander remoto via caminho Cloudflare Access/OAuth **separado do RDP**; em 12/11 mostrou Offline e enrollment ainda pendente. Manter credenciais somente em arquivo privado de usuário (nunca no Git).
- Confirmar GUI de 5 segundos, que VM já ligada conecta diretamente; validar botão Cancelar não cria recursos, reconexão, fallback -Desligar, que disco/imagem permanecem e que VM reinicia preservando dados. Validar poweroff do Linux, transição GCE para TERMINATED e remoção de NAT via reconciliador externo.
- VM desligada ainda gera custos de disco/imagem/staging; teto de R$560 não é enforcement automático. Verificar o painel de faturamento real e remover o staging após liberação.
- Evitar habilitar heartbeats/automação autônoma sem autorização explícita.

### Referências oficiais

- https://docs.cloud.google.com/iap/docs/using-tcp-forwarding
- https://docs.cloud.google.com/sdk/gcloud/reference/compute/routers/nats
- https://docs.cloud.google.com/sdk/gcloud/reference/compute/instances/add-metadata
- https://github.com/DeterminateSystems/nix-installer/releases/tag/v3.24.2
