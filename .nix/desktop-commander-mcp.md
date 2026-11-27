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

### Arquitetura canônica: unidades de sistema sem wrapper

O runtime e o túnel são unidades do gerenciador systemd de sistema,
executadas **com a identidade não privilegiada da workstation**.
`PrivateUsers=yes` mapeia somente root e o UID/GID do serviço; não
use mapeamento full/identity nem execute o backend como root.
`CapabilityBoundingSet=`, `AmbientCapabilities=`, `NoNewPrivileges=yes`,
`ProtectSystem=strict`, `PrivateTmp=yes` e runtime privado permanecem.
O servidor recebe exceções de escrita somente para HOME e runtime da
sessão; o túnel mantém `ProtectHome=read-only`. O login linger e o
user manager fornecem o runtime de sessão e D-Bus. O serviço de
sistema não usa specifiers %h/%U como se fossem os do usuário do serviço.

O gerador versionado `.nix/desktop-commander-systemd.py` é a fonte
única das duas unidades, do fragmento OpenSSH e do ponto de montagem.
O serviço usa o OpenSSH nativo e a política original de usuário/site,
sem lista por provedor ou override Git. O fragmento root-owned
`/etc/desktop-commander-mcp/ssh-client-policy.conf` é montado somente
para leitura no servidor em
`/etc/ssh/ssh_config.d/99-desktop-commander-mcp.conf`. Fora do
namespace, esse último arquivo permanece vazio. O fragmento define
defaults não interativos: BatchMode, StrictHostKeyChecking, sem
atualização automática de known_hosts e sem encaminhamentos. Ele
vem depois do include stock do systemd; políticas explícitas seguem
a precedência nativa do OpenSSH. A validação de aceite exige política
efetiva estrita nos destinos Git testados. Não representa uma barreira
contra mudanças deliberadas da configuração SSH pelo operador.

A configuração original do SSH, known_hosts, chaves e política de
sessões XRDP/SSH não são modificados. O controlador, porta loopback,
autenticação Cloudflare Access e perfil privado de valores não mudam.

### Instalação, verificação e recuperação

Para um novo guest Arch, o bootstrap CI-admitido extrai o instalador
do SHA exato para um diretório temporário administrativo. O subcomando
`install-fresh` cria exclusivamente artefatos ausentes, sem sobrescrever
configurações, alterar modos existentes ou ativar unidades. Recusa
diretórios de sistema inseguros, conflitos e unidades antigas do usuário.
O bootstrap habilita as duas unidades de sistema após a instalação
do perfil e linger; não exige enrollment Cloudflare para instalar o
servidor. O túnel permanece condicionado ao perfil privado.
Não execute o bootstrap de novo em uma workstation já provisionada.

Para revisão sem alterações administrativas, como usuário local:

```bash
stage="$(mktemp -d)"
rmdir "$stage"
python3 .nix/desktop-commander-systemd.py render "$stage"
systemd-analyze verify "$stage/"*.service
```

Para manutenção de um guest existente, compare os artefatos renderizados
com os instalados e o SHA aprovado pelo CI. Alterações administrativas
e restart exigem sessão XRDP/SSH autorizada e plano de recuperação.
Nunca inicie dois backends nas mesmas portas. A aceitação real é feita
dentro do próprio conector MCP, não apenas na sessão XRDP:

```bash
python3 .nix/desktop-commander-systemd.py probe "$HOME" /example/checkout
```

Esse probe valida os mapas mínimos, identidade não root, capacidades
vazias, NoNewPrivileges, mounts protegidos, fragmento exato read-only,
HOME/runtime graváveis e OpenSSH nativo. Verifica a política efetiva
com ssh -G antes da conexão e usa `git ls-remote origin HEAD` normalmente.
Exigir PASS para dois checkouts SSH conhecidos. O comando
`admin-probe` permite a prova transitória equivalente em XRDP, com
os artefatos de política já presentes; não inicia o MCP, abre portas
ou substitui o teste do conector autenticado. Não aprovar chaves de
hosts desconhecidos automaticamente nem reduzir a verificação.

Em recuperação, preserve uma sessão XRDP/SSH e os artefatos
administrativos exatos anteriormente aprovados. A interrupção do
backend encerra a conexão e os processos da ferramenta; verifique
o estado antes de repetir qualquer operação. O antigo caminho de
serviços de usuário e shim foi retirado da árvore ativa após o aceite
operacional. Seu rollback histórico está na revisão
`f002b75eb0110a15afc55a60765cbaef9d921ace`: recuperar somente as duas
unidades de usuário e os dois arquivos do shim dessa revisão, em
isolamento e com revisão de hashes, sem retroceder outros commits.
Para esse rollback explícito, parar/desabilitar primeiro o par de
sistema, retirar apenas o fragmento e o ponto de montagem novos,
restaurar os quatro artefatos históricos aos caminhos originais e
reativar o par de usuário. O shim histórico rejeita includes adicionais,
inclusive o anchor vazio. Nunca alterar o include stock nem relaxar
as chaves. Esse caminho é apenas recuperação, não instalação normal.

No destino NixOS, declarar a mesma arquitetura em módulos
`systemd.services` com caminhos nativos de NixOS e testes equivalentes;
não portar o instalador imperativo específico do guest Arch.

## Operação

```bash
systemctl status desktop-commander-mcp-runtime.service desktop-commander-mcp-tunnel.service
journalctl -u desktop-commander-mcp-runtime.service -u desktop-commander-mcp-tunnel.service
desktop-commander-mcp-runtime check
desktop-commander-mcp-runtime probe
```

Não copie tokens, conteúdo de ferramentas ou logs para tickets/chat; mantenha journals privados. Se o serviço local estiver saudável e o probe falhar, verifique DNS/TLS, túnel, hostname, policy Access, audience e discovery sem imprimir credenciais. Para revogar acesso, pare os serviços, revogue sessões/tokens Access e remova a identidade da policy; apagar somente o token administrativo não revoga tokens de túnel já emitidos. Remova dados temporários privados quando deixarem de ser necessários.

As cotas do ChatGPT e limites do Cloudflare continuam aplicáveis. A conexão é para o Chat normal; ferramentas, raciocínio, tráfego e arquivos grandes estão sujeitos às políticas e limites dos serviços envolvidos. Desktop Commander stdio não é acessível pelo ChatGPT Web; gateways OAuth próprios são uma alternativa com custo adicional de manutenção.
