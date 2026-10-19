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

## Operação

```bash
systemctl --user status desktop-commander-mcp-runtime.service desktop-commander-mcp-tunnel.service
journalctl --user -u desktop-commander-mcp-runtime.service -u desktop-commander-mcp-tunnel.service
desktop-commander-mcp-runtime check
desktop-commander-mcp-runtime probe
```

Não copie tokens, conteúdo de ferramentas ou logs para tickets/chat; mantenha journals privados. Se o serviço local estiver saudável e o probe falhar, verifique DNS/TLS, túnel, hostname, policy Access, audience e discovery sem imprimir credenciais. Para revogar acesso, pare os serviços, revogue sessões/tokens Access e remova a identidade da policy; apagar somente o token administrativo não revoga tokens de túnel já emitidos. Remova dados temporários privados quando deixarem de ser necessários.

As cotas do ChatGPT e limites do Cloudflare continuam aplicáveis. A conexão é para o Chat normal; ferramentas, raciocínio, tráfego e arquivos grandes estão sujeitos às políticas e limites dos serviços envolvidos. Desktop Commander stdio não é acessível pelo ChatGPT Web; gateways OAuth próprios são uma alternativa com custo adicional de manutenção.
