{ pkgs, desktopCommanderRuntime }:
let
  # Bump note: mcp-proxy/cloudflared follow the locked nixpkgs input. After
  # updating it, check proxy CLI/Streamable HTTP session behavior, cloudflared
  # originRequest.access validation, and run make test plus the public OAuth probe.
  python = pkgs.python3.withPackages (p: [ p.mcp ]);
  controller = pkgs.replaceVars ./desktop-commander-mcp.py {
    smoke = ./tests/desktop-commander-mcp-smoke.py;
    publicProbe = ./tests/desktop-commander-mcp-public-probe.py;
  };
  runtime = pkgs.writeShellApplication {
    name = "desktop-commander-mcp-runtime";
    runtimeInputs = [
      pkgs.mcp-proxy
      pkgs.cloudflared
      desktopCommanderRuntime
      pkgs.coreutils
    ];
    text = ''
      export DESKTOP_COMMANDER_MCP_EXECUTABLE="$0"
      exec ${python}/bin/python3 ${controller} "$@"
    '';
  };
in
pkgs.buildEnv {
  name = "desktop-commander-mcp-runtime";
  paths = [
    runtime
    pkgs.cloudflared
  ];
}
