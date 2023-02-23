{pkgs, ...}: let
  maelstrom-src = builtins.fetchTarball {
    url = "https://github.com/jepsen-io/maelstrom/releases/download/v0.2.2/maelstrom.tar.bz2";
    sha256 = "sha256:0haqa2ax9pqk4m0ckax7wl2cmsv49nvjw8862q7pd5xm9dn1k2dz";
  };
in
  pkgs.writeShellScriptBin "maelstrom" ''
    exec ${pkgs.jdk17}/bin/java -Djava.awt.headless=true -jar ${maelstrom-src}/lib/maelstrom.jar "$@"
  ''
