# Desktop Commander MCP para ChatGPT

## Arquitetura e limites

O runtime combina Desktop Commander `0.2.52` (MIT), `mcp-proxy` `0.12.0` e `cloudflared` `2026.9.3`, fornecidos pelo `nixpkgs` locked. Não há servidor MCP próprio nem relay Desktop Commander Remote: o proxy expõe o backend existente em `http://127.0.0.1:18879/mcp`, em loopback. O acesso local não tem autenticação e deve ser considerado confiável.

O `cloudflared` publica o túnel e exige JWT do Cloudflare Access com a audience configurada; o Access aplica Managed OAuth e RFC 8707. O hostname autorizado é `host.example.invalid`, coberto pelo Universal SSL ativo de `*.solosig.com.br`. O nome inicialmente considerado (`host.example.invalid`) foi descartado porque o wildcard não cobre dois níveis. Não foi comprado certificado ou produto.

Há um único backend persistente. Conexões HTTP MCP stateless podem reconectar após reiniciar o servidor, mas processos/comandos iniciados por ferramentas não sobrevivem à perda do backend. Faça polling de processos longos; após reinício, inicie-os novamente.

Desktop Commander recebe o mesmo usuário e `HOME` do serviço local. A configuração lida é `~/.claude-server-commander/config.json`. `allowedDirectories` restringe o comportamento esperado da ferramenta de arquivos, **não** é sandbox do terminal, contenção completa do filesystem nem isolamento contra prompt injection. Não prometa essas propriedades. Logs da ferramenta podem conter argumentos e resultados, tal como no Desktop Commander existente; mantenha arquivos de configuração/credenciais privados e não compartilhe logs ou segredos.

Por decisão do usuário, o ambiente real preserva `allowedDirectories: []` (sem restrição de arquivos), em vez de acrescentar um sandbox novo. Clientes autorizados podem alcançar arquivos privados com as permissões de `klarkc`. As confirmações do ChatGPT não constituem uma aprovação obrigatória imposta pelo servidor para todo comando destrutivo. A saída de processos é combinada: o teste preserva conteúdo de stdout/stderr e código de saída, não dois streams separados.

## Preparação local após integração autorizada

Depois de integrar a alteração na `main` e atualizar o perfil a partir de `/home/example`:

```bash
nix profile upgrade klarkc
systemctl --user daemon-reload
systemctl --user enable --now desktop-commander-mcp-runtime.service desktop-commander-mcp-tunnel.service
```

O runtime escuta em `127.0.0.1:18879`. O túnel só inicia quando existe a configuração privada. O serviço Remote existente não precisa ser alterado.

Use um token de API Cloudflare carregado localmente de `$HOME/.profile_override` para configurar/criar o túnel e a aplicação Access, em vez de login com certificado. Não passe tokens em argumentos, não os grave em arquivos versionados/Nix e não os cole no chat ou em logs. Guarde a configuração em `~/.profile_override.desktop-commander-mcp/config.json` e o arquivo gerado `credentials.json` em diretório privado, modo `0600`; o diretório deve ser privado. O comando `configure` valida o arquivo de credenciais, cria apenas a configuração local e **não** publica DNS nem inicia o túnel.

O bootstrap usa um API token carregado de `.profile_override`, com `CLOUDFLARE_API_TOKEN`, `CLOUDFLARE_ACCOUNT_ID` e `CLOUDFLARE_ACCESS_EMAIL`. `CLOUDFLARE_ZONE_ID` é opcional: a zona exata é descoberta pela API e validada contra a conta. Permissões: Zone Read, DNS Edit, SSL and Certificates Read para `solosig.com.br`; Account Cloudflare Tunnel Edit, Access Apps and Policies Edit e Access Organizations Read para a conta selecionada. O token administrativo não é importado para o serviço.

```bash
python3 .nix/desktop-commander-mcp-cloudflare.py inspect
python3 .nix/desktop-commander-mcp-cloudflare.py provision --hostname host.example.invalid
```

O provisionamento cria somente recursos dedicados chamados `host-example-desktop-commander-mcp`; verifica policy e Managed OAuth antes de publicar o DNS. Recusa sobrescrever recursos conflitantes. Checkpoints e credenciais ficam no diretório privado; uma reexecução verifica recursos existentes sem duplicá-los. Não inicia serviços. Se preferir configurar manualmente, obtenha o tunnel ID, nome curto da equipe Zero Trust e a audience de 64 caracteres (`AUD` da aplicação Access), substituindo os placeholders:

```bash
umask 077
desktop-commander-mcp-runtime configure \
  --hostname host.example.invalid \
  --tunnel-id '<TUNNEL_UUID>' \
  --team-name '<CLOUDFLARE_TEAM_NAME>' \
  --audience '<ACCESS_AUD_TAG_64_HEX>' \
  --credentials-file "$HOME/.profile_override.desktop-commander-mcp/credentials.json"
desktop-commander-mcp-runtime check
```

Os valores acima são placeholders, não IDs ou credenciais reais. O túnel configura validação de Access JWT obrigatória para o hostname exato e rejeita hosts não mapeados. Restrinja a política Access à identidade do usuário; MFA pelo IdP é recomendado. Não crie políticas Bypass. Prefira token de acesso curto (15 minutos) e duração de refresh longa, de acordo com as opções suportadas pelo Access.

Na implantação validada, a aplicação permite somente o email pessoal informado em `CLOUDFLARE_ACCESS_EMAIL` e somente o provedor One-time PIN. O PIN é login por email, não MFA independente; proteja a caixa de email com MFA. O cadastro/seleção do provedor é feito em Zero Trust → Integrations → Identity providers e na aplicação Access, separadamente do bootstrap. Configurá-lo pela API requer temporariamente Access Organizations, Identity Providers, and Groups Edit. Não é necessário criar usuário administrador Cloudflare para o email pessoal. Mudar a variável local não altera uma policy já criada: atualize explicitamente a regra Email dessa aplicação, sem liberar Everyone ou um domínio inteiro. O verificador recusa drift em vez de sobrescrever a política silenciosamente.

## Configuração no ChatGPT

No ChatGPT atual, abra **Plugins → Add custom MCP server** e configure OAuth usando a URL pública:

```text
https://host.example.invalid/mcp
```

Complete o login Managed OAuth/Cloudflare Access com a identidade permitida. Na configuração OAuth do provedor, permita **somente** o callback exato mostrado pela página de gerenciamento do ChatGPT. Exemplos documentados, sujeitos ao callback exibido para a conexão:

- `https://chatgpt.com/connector_platform_oauth_redirect`
- `https://chatgpt.com/connector/oauth/{callback_id}`

O bootstrap permite somente o callback estável e o padrão `https://chatgpt.com/connector/oauth/*` para os IDs específicos de conexão do ChatGPT. Se possível, restrinja depois ao callback exato mostrado no seu conector. Não permita domínios ou padrões adicionais. Use o ChatGPT normal na Web; este procedimento não configura Work ou Codex. Limites de uso e disponibilidade de ferramentas são separados e podem variar: não há garantia de uso ilimitado nem de chamada instantânea; o ChatGPT pode usar raciocínio automático.

## Verificação e estado atual

| Verificação                                                | Resultado                                                                  |
| ---------------------------------------------------------- | -------------------------------------------------------------------------- |
| Smoke local do MCP/protocolo, ferramentas e reconexão HTTP | Aprovado; não exercita OAuth                                               |
| Wrapper `api-token` e `oauth`                              | Ambos executam o mesmo smoke local; nenhum simula OAuth real               |
| Probe público                                              | Aprovado: TLS, rejeição sem/invalid bearer, discovery OAuth e PKCE         |
| Parada e recuperação dos serviços de validação             | Aprovado no systemd; instalação persistente autorizada após os testes      |
| OAuth e comandos originados no ChatGPT Web                 | Aprovado pelo usuário; respostas 200/202 observadas no túnel dedicado      |
| Leitura, gravação e releitura pelo ChatGPT                 | Aprovado: proof.txt conferido no host contra nonce não exibido previamente |
| Processo longo pelo ChatGPT                                | Aprovado: 15,12 s, polling por PID, stdout/stderr presentes e exit code 7  |
| Allowlist de arquivos                                      | Aprovada no smoke isolado; produção irrestrita por decisão do usuário      |
| Renovação OAuth após expirar o access token                | Não validada ponta a ponta; reautenticar se refresh falhar                 |

Rode `desktop-commander-mcp-runtime probe` para repetir o teste público. Ele não completa login nem testa uma chamada real do ChatGPT. O probe identifica seu User-Agent: a borda Cloudflare rejeitou o User-Agent genérico de urllib com 403, mas retornou o desafio OAuth correto ao probe identificado. Não foi desabilitado WAF ou outro controle para contornar isso.

Para o teste manual, `desktop-commander-mcp-runtime prepare` cria `~/.profile_override.desktop-commander-mcp/smoke/challenge.txt`, com nonce aleatório não exibido. Peça ao MCP no Chat normal para ler esse arquivo, copiar suas linhas para `proof.txt` na mesma pasta e acrescentar uma linha `verified-from-chatgpt`. Peça para reler a prova. `desktop-commander-mcp-runtime verify` compara a prova com o nonce original. Confirme também o histórico de ferramentas na sessão do ChatGPT: o nonce comprova acesso ao host, mas não identifica sozinho qual cliente o acessou.

Valide `hostname`, `nix --version`, ferramentas Git/worktree numa pasta de teste nova e um comando que dure 15 segundos, imprima stdout/stderr e termine com código 7. Acompanhe pelo PID, incluindo uma reconexão HTTP. Não use agentes pagos nem altere a worktree do navegador. Reiniciar o servidor encerra processos antigos: inicie-os de novo após o reinício.

Antes da integração na `main`, o endpoint usa unidades transitórias `desktop-commander-mcp-staging.service` e `desktop-commander-mcp-tunnel-staging.service`, com o runtime compilado da worktree. Elas não iniciam no próximo boot e desaparecem quando paradas. Não são substitutos para a instalação definitiva; essa instalação depende de autorização e `nix profile upgrade klarkc` na home.

Em 2026-10-16, o usuário concluiu OAuth e os testes reais do ChatGPT Web e autorizou a integração. A prova foi comparada no host antes da integração. Não há credencial ou nonce de teste versionado neste documento. O antigo conector pode ser confundido com o novo: use a URL acima, confira as ferramentas diretas `read_file`/`start_process` e não o fluxo de dispositivos/ping em `mcp.desktopcommander.app`.

## Custos e alternativas

Não há cobrança por chamada do Desktop Commander Remote, Worker Cloudflare ou API de modelo por trás do runtime. Foram usados o domínio e os serviços Cloudflare existentes, sem compra de certificado ou produto. O plano gratuito e os termos da Cloudflare continuam impondo condições de uso, disponibilidade e limites de infraestrutura; o domínio também tem sua renovação habitual. Isso não é uma promessa de tráfego ilimitado ou de ausência de dependência externa.

As cotas e disponibilidade de modelos/ferramentas do ChatGPT continuam valendo. A conexão é para o Chat normal, sem iniciar Work, Codex ou OpenCode; não foi feita medição da contabilização interna da OpenAI. Raciocínio automático, limites de ferramentas e políticas do workspace podem afetar o uso. Operações longas devem retornar um PID e ser acompanhadas por polling, evitando timeouts HTTP. Grandes arquivos/tráfego de mídia também estão sujeitos aos termos do proxy Cloudflare.

Desktop Commander apenas em stdio não é acessível pelo ChatGPT Web. O Remote do fornecedor tem a cota que motivou a substituição. OVPN sozinho não conecta o serviço Web da OpenAI à VPN. Um gateway FastMCP com OAuth próprio é alternativa se Managed OAuth deixar de atender, mas adiciona manutenção. Work/Codex/Dot não substituem este caminho de uso no Chat normal. Não foi criado servidor de ferramentas próprio.

## Operação, revogação e recuperação

```bash
systemctl --user status desktop-commander-mcp-runtime.service desktop-commander-mcp-tunnel.service
journalctl --user -u desktop-commander-mcp-runtime.service -u desktop-commander-mcp-tunnel.service
desktop-commander-mcp-runtime check
desktop-commander-mcp-runtime probe
```

Não copie tokens, credenciais, conteúdo de ferramentas ou logs para tickets/chat. Mensagens locais ocultam valores de configuração; mantenha também os journals privados. Se o serviço local estiver saudável mas o probe público falhar, verifique DNS/TLS, túnel, hostname, política Access, audience e discovery OAuth sem imprimir credenciais. Confirme novamente a cobertura TLS antes de alterar DNS.

Para revogar o acesso, pare os dois serviços e revogue sessões/tokens no Cloudflare Access; remova a identidade da política de allow. Apagar/desabilitar apenas o token de API administrativo não revoga tokens de túnel já emitidos. Remova os arquivos privados de configuração/credenciais quando não forem mais necessários. Para recuperar, corrija a configuração local com permissões privadas e reinicie os serviços; não exponha credenciais em argumentos ou logs.

```bash
# Instalação definitiva:
systemctl --user disable --now desktop-commander-mcp-tunnel.service desktop-commander-mcp-runtime.service
# Durante a validação anterior à integração:
systemctl --user stop desktop-commander-mcp-tunnel-staging.service desktop-commander-mcp-staging.service
```
