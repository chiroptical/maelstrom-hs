{pkgs, ...}:

let 
  maelstrom-src = pkgs.fetchurl {
    url = "https://github.com/jepsen-io/maelstrom/releases/download/v0.2.2/maelstrom.tar.bz2";
    sha256 = "sha256-N5UGfpG5FqKxHRbrzhAjMsHw9LM1EHw6MI62r0NNImA=";
    downloadToTemp = true;
    postFetch = ''
      mkdir $out
      tar xvf $downloadedFile -C $out
    '';
  };
in
  pkgs.writeShellScriptBin "maelstrom" ''
    exec ${pkgs.jdk17}/bin/java -Djava.awt.headless=true -jar ${maelstrom-src}/lib/maelstrom.jar "$@"
  ''
