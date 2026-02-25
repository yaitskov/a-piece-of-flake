# a-piece-of-flake

The web service provides a Nix flake repository that aims to provide
smooth experience for publishing flakes.

## Service
The service is already deployed at [a-piece-of-flake](https://localhost:3001)

## Development environment

```shell
$ nix develop
$ emacs &
$ cabal build
$ cabal test
```

## Release
```shell
$ nix build
$ ./result/bin/a-piece-of-flake run
```


## Installation

### NixOS module

#### NixOS with flakes

Modify `/etc/nixos/flake.nix` as follows:

``` nix
  # ...
  inputs = {
    # ...
    a-piece-of-flake.url = "github:yaitskov/a-piece-of-flake";
  };
```
``` nix
  # ...
        modules = [
          a-piece-of-flake.nixosModules.${system}.default
          ({ ... }: {
            programs.a-piece-of-flake = {
              enable = true;
              port = 3000;
            };
          })
          ./configuration.nix
        ];
```

#### NixOS without flakes
``` nix
  let
    pof = builtins.fetchGit "htts://github.com/yaitskov/a-piece-of-flake.git?ref=master";
  in {
  imports =
    [ # ... ./hardware-configuration.nix
      "${pof}/nixos/non-flake-lfi.nix"
    ];

```

``` nix
  programs = {
    a-piece-of-flake = {
      port = 3000;
      enable = true;
    };
  };
```
