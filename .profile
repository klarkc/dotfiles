# Load user profile overrides (values only: no executable logic).
if [ -f "$HOME/.profile_override" ]; then
  . "$HOME/.profile_override"
fi

# Derive the Atlassian MCP Basic credential at shell startup, never in .profile_override.
if [ -n "${ATLASSIAN_USER_EMAIL:-}" ] && [ -n "${ATLASSIAN_MCP_TOKEN:-}" ]; then
  ATLASSIAN_MCP_BASIC_AUTH="$(
    printf '%s' "${ATLASSIAN_USER_EMAIL}:${ATLASSIAN_MCP_TOKEN}" | base64 -w0
  )"
  export ATLASSIAN_MCP_BASIC_AUTH
else
  unset ATLASSIAN_MCP_BASIC_AUTH
fi
