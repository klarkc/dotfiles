{ pkgs, opencode-src }:

pkgs.opencode.overrideAttrs (old: {
  src = opencode-src;

  node_modules = old.node_modules.overrideAttrs (_: {
    src = opencode-src;
    outputHash = "sha256-4yjQlxN+U4CKwA/hE8gACuvA4bBeTrX0ACVBIK4UQCg=";
  });
})
