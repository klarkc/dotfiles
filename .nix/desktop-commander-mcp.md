# Desktop Commander MCP para ChatGPT

## Arquitetura e limites

O runtime combina Desktop Commander, `mcp-proxy` e `cloudflared` fornecidos pelo `nixpkgs` locked. O proxy expõe o backend existente em loopback; o serviço local não tem autenticação e deve ser considerado confiável. O túnel exige JWT do Cloudflare Access com a audience configurada. Há um único backend persistente: clientes HTTP MCP stateless podem reconectar, mas processos iniciados por ferramentas não sobrevivem à perda do backend; faça polling de processos longos e reinicie-os após reinício do serviço.

Desktop Commander recebe o mesmo usuário e `HOME` do serviço local. `allowedDirectories` restringe o comportamento esperado da ferramenta de arquivos, mas não é sandbox do terminal nem isolamento completo de filesystem ou prompt injection. Logs podem conter argumentos e resultados; mantenha-os privados. A configuração de produção já autorizada preserva `allowedDirectories: []`; não afirme que há uma barreira de aprovação do servidor para comandos destrutivos. Saídas de processos são combinadas, não streams separados.

## Configuração e operações planejadas

Toda configuração privada é lida de `$HOME/.profile_override`; não crie arquivos manuais irmãos ou `config.json`/credenciais persistentes. Os subcomandos `check`, `probe` e `tunnel` do controller devem ler desse ambiente:

```text
DESKTOP_COMMANDER_MCP_HOSTNAME
CLOUDFLARE_ZONE_NAME
DESKTOP_COMMANDER_MCP_RESOURCE_NAME
CLOUDFLARE_TUNNEL_ID
CLOUDFLARE_ACCESS_TEAM
CLOUDFLARE_ACCESS_AUDIENCE
CLOUDFLARE_TUNNEL_SECRET
CLOUDFLARE_ACCOUNT_ID
```

O bootstrap de administração `inspect`/`provision` deriva os recursos dessas mesmas configurações. IDs de checkpoint `CLOUDFLARE_ACCESS_APP_ID` e `CLOUDFLARE_MCP_DNS_RECORD_ID`, quando existentes, também ficam somente em `$HOME/.profile_override`. Variáveis administrativas existentes `CLOUDFLARE_API_TOKEN`, `CLOUDFLARE_ACCESS_EMAIL` e `CLOUDFLARE_ZONE_ID` (opcional) são apenas para bootstrap; o runtime não usa nem importa o token administrativo.

O subcomando antigo `configure` deve ser removido: configuração e correções são feitas no ambiente privado. Não passe hostname privado em argumentos CLI. Quando uma ferramenta exigir JSON de credencial, `tunnel` pode gerá-lo a partir do ambiente em `$XDG_RUNTIME_DIR`, com diretório `0700` e arquivo `0600`; nunca persista credenciais ou valores reais em arquivos versionados ou no Nix store. Arquivos de smoke/challenge/proof são dados temporários de teste, não configuração, e não devem conter nonces, PIDs ou metadados reais em documentação.

O serviço local escuta em loopback; o túnel só deve iniciar com as variáveis necessárias presentes. O bootstrap deve validar recursos existentes, evitar duplicação e recusar drift/conflictos em vez de sobrescrever silenciosamente. Não alegue que uma implantação ou novo teste foi executado sem evidência.

## Cliente e segurança de acesso

No ChatGPT, configure um MCP remoto com `https://mcp.example.com/mcp` como exemplo reservado; substitua pelo hostname carregado localmente, sem publicá-lo. Use Managed OAuth/Cloudflare Access e permita somente o callback exato apresentado pela interface. Restrinja a policy à identidade autorizada; não use Bypass nem libere um domínio inteiro. PIN por email não é MFA independente. A configuração do cliente não cobre Work ou Codex. Não prometa uso ilimitado nem disponibilidade constante.

`probe` verifica o endpoint público, TLS, rejeição de bearer ausente/inválido e discovery OAuth/PKCE; não conclui login nem comprova uma chamada real do ChatGPT. Smoke local verifica protocolo/ferramentas/reconexão, não OAuth. Evidência E2E anterior confirmou OAuth, chamada e leitura/escrita/releitura pelo ChatGPT Web, além de processo longo acompanhado por polling; renovação OAuth após expiração não foi validada ponta a ponta. Essa evidência não autoriza divulgar domínio, nonce, PID, identidade ou metadados de execução.

Para processo longo, recolha o PID retornado e faça polling; após reinício do backend, inicie novamente. O probe pode requerer User-Agent compatível com a borda; não desabilite WAF nem controles para contornar rejeições. Não use a integração antiga do fornecedor quando verificar o novo MCP: confira as ferramentas diretas do conector configurado.

## Git via SSH no runtime isolado

Em serviços systemd de usuário, `ProtectSystem=strict` e `PrivateTmp=yes`
podem implicar um user namespace sem mapeamento do UID/GID 0. Arquivos de
configuração SSH protegidos do root então aparecem como `nobody`, e o OpenSSH
pode falhar com `Bad owner or permissions` antes de abrir a conexão. Isso
não indica necessariamente permissões erradas no host. Não altere ownership
em /etc, nem desligue o hardening.

### Migração estrutural para unidades de sistema — candidato, não ativado

A solução de destino é executar os dois serviços pelo gerenciador systemd
de sistema, **com o mesmo usuário não privilegiado**. `PrivateUsers=yes`
configura o mapeamento mínimo de root e do usuário/grupo do serviço; não
use mapeamento full/identity nem execute o backend como root.
`CapabilityBoundingSet=`, `AmbientCapabilities=`, `NoNewPrivileges=yes`,
`ProtectSystem=strict`, `PrivateTmp=yes` e runtime privado permanecem.
Apenas o HOME e o runtime da sessão do usuário recebem exceções de escrita
necessárias no servidor; o túnel mantém `ProtectHome=read-only`.
O login linger e o user manager continuam responsáveis pela sessão/D-Bus.
O serviço de sistema não usa os specifiers %h/%U do gerenciador como se
fossem os do usuário do serviço.

O gerador versionado `.nix/desktop-commander-systemd.py` produz as duas
unidades completas para a conta local não root, com o PATH normal e sem
wrapper. Não instala nada automaticamente, não altera a configuração SSH
nem cria arquivos privados de configuração. O controlador, porta loopback,
validação JWT do Cloudflare Access e arquivo privado de valores continuam
os mesmos. O OpenSSH volta a ler a configuração nativa, inclusive a
política de usuário/site e os known_hosts existentes. O padrão nativo
pode ser `StrictHostKeyChecking ask`, `BatchMode no` e
`UpdateHostKeys yes`, diferentes das garantias do shim anterior.

Para preservar essas garantias, o gerador produz também um fragmento
OpenSSH de defaults não interativos: verificação estrita das chaves,
BatchMode, sem atualização automática de known_hosts e sem encaminhamentos.
A unidade do servidor (não a do túnel) monta esse arquivo somente para
leitura em `/etc/ssh/ssh_config.d/99-desktop-commander-mcp.conf`.
No host existe apenas um arquivo vazio como ponto de montagem. A fonte
root-owned fica em `/etc/desktop-commander-mcp/ssh-client-policy.conf`.
Assim, não há mudança de opções para a sessão XRDP/SSH normal, lista de
provedores, substituição do ssh_config global, shim no PATH ou override
das opções Git. Os defaults vêm depois da configuração stock do systemd;
políticas explícitas do usuário/site continuam autoritativas segundo a
precedência normal do OpenSSH. A validação de aceite falha se as opções
efetivas dos destinos Git testados não forem estritas. Isso não transforma
o arquivo de defaults em uma barreira contra mudanças deliberadas da
política SSH pelo operador.

O arquivo vazio adicional é incompatível com a validação stock do shim
antigo, que rejeita includes adicionais. Por isso, não o deixe presente
durante o rollback. Na prova transitória, sua presença deve ser limitada
à duração do teste e pode interromper temporariamente o Git pelo MCP
legado, embora não interrompa seus serviços.

**Etapa 1 — prova privilegiada sem cutover.** Em uma sessão XRDP/SSH
autorizada, como o usuário da workstation, gere e revise os artefatos
em um diretório novo privado. Não sobrescreva arquivos de sistema
preexistentes. Para o teste, instale temporariamente o fragmento e o
ponto de montagem vazios com owner root, modo 0644; o diretório dedicado
usa modo 0755. Não altere permissões, ownership ou conteúdo de arquivos
SSH preexistentes.

```bash
stage="$(mktemp -d)"
rmdir "$stage"
python3 .nix/desktop-commander-systemd.py render "$stage"
systemd-analyze verify "$stage/"*.service
```

Depois de revisar os artefatos, o operador cria exclusivamente o
diretório dedicado e os dois arquivos temporários: copiar o fragmento
gerado para o caminho fonte e criar o ponto de montagem vazio. Recusar
qualquer caminho já existente, inclusive symlinks quebrados, antes
da criação; usar criação exclusiva, sem sobrescrever. Registrar quais
arquivos foram efetivamente criados. Após o teste, remover somente esses
artefatos, mesmo se o probe falhar. Não remover arquivos alheios nem
modificar o include original. Executar o probe como usuário da sessão:

```bash
python3 .nix/desktop-commander-systemd.py admin-probe "$HOME" /example/checkout
```

O subcomando admin-probe usa sudo para criar uma unidade systemd
transitória com os mesmos parâmetros de identidade e hardening do
servidor proposto. O processo roda sem privilégios, sem instalar
serviços, abrir portas, alterar gates ou escrever nos repositórios.
Exigir exit 0, mapeamentos mínimos UID/GID, include original root-owned,
fragmento exato montado read-only, mounts protegidos, capacidades
vazias, HOME/runtime acessíveis e PASS para dois checkouts SSH.
O probe verifica a configuração efetiva com ssh -G (sem conexão),
recusa política mais fraca antes de qualquer conexão e executa
`git ls-remote origin HEAD` normalmente. Não modifica variáveis de
transporte. Falhas não exibem URLs nem credenciais. Confirmar a remoção
dos dois arquivos temporários e a recuperação do Git pelo MCP anterior.
Testes unitários e o probe transitório não equivalem ao teste do
conector MCP autenticado.

**Etapa 2 — preparar e revisar unidades.** Somente depois da prova acima,
hooks, CI e revisão do SHA exato, gerar os arquivos em um diretório novo
privado e verificar sua sintaxe:

```bash
stage="$(mktemp -d)"
rmdir "$stage"
python3 .nix/desktop-commander-systemd.py render "$stage"
systemd-analyze verify "$stage/"*.service
```

Revisar o conteúdo antes de uma instalação administrativa explícita.
Não instalar sobre unidades de sistema já existentes sem reconciliar
a origem. O gerador não é executado como root. O operador copia os
dois arquivos revisados para /etc/systemd/system, com owner root e modo
0644, e recarrega o system manager. Instala também o fragmento e ponto
de montagem vazios exatos quando estiver pronto para iniciar o novo
par, seguindo a mesma validação de caminhos da etapa 1. A ativação
é separada, com sessão XRDP/SSH mantida para rollback. Evitar qualquer
execução simultânea das instâncias de usuário e de sistema, que
disputariam as mesmas portas.

**Etapa 3 — cutover e aceite.** Parar/desabilitar as unidades antigas do
usuário somente quando o novo par estiver preparado e validado; iniciar
e habilitar o par de sistema. A conexão MCP atual será interrompida.
Reconectar pelo Desktop Commander real e validar as duas operações Git
normais no namespace do backend, a autenticação Cloudflare, ferramentas
MCP, HOME e sessão gráfica, não apenas o status systemd ou o XRDP.
Se houver falha, parar/desabilitar o par de sistema, remover somente
os dois arquivos criados para o fragmento/ponto de montagem (e o
diretório dedicado vazio) e reabilitar o par de usuário anterior.
Conferir que o include stock original permanece intacto. Não alterar
SSH, ACLs, chaves ou hooks para compensar uma falha.

O wrapper existente e as unidades de usuário são preservados nesta
primeira etapa **somente como caminho operacional anterior/rollback**.
O bootstrap existente também permanece no modo anterior até a prova
real do novo par. Remover o wrapper e atualizar bootstrap e runbook
somente após o aceite. O destino definitivo NixOS deve declarar as
unidades via módulos systemd e repetir os testes, sem transportar o
instalador específico do Arch.

### Caminho de usuário anterior (rollback)

O shim atual no PATH do serviço MCP trata apenas o caso estreito de
Git para github.com, namespace sem root mapeado, include stock de
transportes locais do systemd e ausência de configuração SSH de usuário.
Ele usa a configuração de compatibilidade explícita com BatchMode e
StrictHostKeyChecking habilitados. Outros destinos seguem o OpenSSH
nativo, sem substituir política de usuário/site. Não ampliar este
paliativo por provedor na migração estrutural; mantenha-o inalterado
até que o par de sistema tenha sido aceito.

## Operação

```bash
systemctl --user status desktop-commander-mcp-runtime.service desktop-commander-mcp-tunnel.service
journalctl --user -u desktop-commander-mcp-runtime.service -u desktop-commander-mcp-tunnel.service
desktop-commander-mcp-runtime check
desktop-commander-mcp-runtime probe
```

Não copie tokens, conteúdo de ferramentas ou logs para tickets/chat; mantenha journals privados. Se o serviço local estiver saudável e o probe falhar, verifique DNS/TLS, túnel, hostname, policy Access, audience e discovery sem imprimir credenciais. Para revogar acesso, pare os serviços, revogue sessões/tokens Access e remova a identidade da policy; apagar somente o token administrativo não revoga tokens de túnel já emitidos. Remova dados temporários privados quando deixarem de ser necessários.

As cotas do ChatGPT e limites do Cloudflare continuam aplicáveis. A conexão é para o Chat normal; ferramentas, raciocínio, tráfego e arquivos grandes estão sujeitos às políticas e limites dos serviços envolvidos. Desktop Commander stdio não é acessível pelo ChatGPT Web; gateways OAuth próprios são uma alternativa com custo adicional de manutenção.
