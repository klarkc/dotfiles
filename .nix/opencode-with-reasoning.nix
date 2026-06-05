{ pkgs, opencode-src }:

pkgs.opencode.overrideAttrs (old: {
  src = opencode-src;

  node_modules = old.node_modules.overrideAttrs (_: {
    src = opencode-src;
    outputHash = "sha256-ZBdR7Vz4N0aKeXzHI7G70j9vE6hLlDw+Dam5WLruVoI=";
  });
})
