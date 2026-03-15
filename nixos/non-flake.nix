{ config
, lib
, pkgs
, ...
}: let
  pof = (builtins.getFlake "github:yaitskov/a-piece-of-flake").packages.x86_64-linux.a-piece-of-flake;
  #lfi = (builtins.getFlake "path:/home/don/pro/haskell/my/a-piece-of-flake/a-piece-of-flake").packages.x86_64-linux.a-piece-of-flake;
in (import ./flake.nix pof) { inherit config; inherit lib; inherit pkgs; }
