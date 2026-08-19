{
  lib,
  pkgs,
}:

let
  backupRuntime = with pkgs; [
    bzip2
    coreutils
    file
    findutils
    gawk
    gnugrep
    gnused
    gnutar
    gzip
    lrzip
    p7zip
    procps
    rsync
    unzip
    util-linux
    xz
    zip
    zstd
  ];

  packScript = pkgs.writeShellApplication {
    name = "archive-pack";
    runtimeInputs = backupRuntime;
    text = builtins.readFile ./archive-pack/archive-pack.sh;

  };

  testScript = pkgs.writeShellApplication {
    name = "archive-pack-test";
    runtimeInputs = backupRuntime ++ [ packScript ];
    text = builtins.readFile ./archive-pack/archive-pack-test.sh;
  };
in
{
  inherit packScript testScript;
  backupRuntime = backupRuntime;
}
