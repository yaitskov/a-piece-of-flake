# a-piece-of-flake

The main idea behind this project is to provide a quick and simple
interface for publishing Nix flakes.

Flakes have become relatively mature and address the central
repository issue. However, the Nixpkgs repository on GitHub still has
more that 5k open issues and a comparable number of pull requests, and
continues to receive many commits every day.  Getting a pull request
for a new tool merged into Nixpkgs can be difficult - the Nixpkgs
README explicitly discourages people from submitting their "pet"
projects.

The Nixpkgs repository is huge. It contains more than 120k packages, but
the majority of them are not native to Nix. For example, about 10%
are Haskell packages imported. Therefore, this large number cannot be
used as a reliable measure of how well the publishing process is
developed in Nix. For instance, the PyPy repository alone currently
contains almost 900k packages.

It is also important to note Python is the most popular
general-purpose programming language, and its publishing process was
designed by programmers for programmers. Yet there is no pull-request
step in the workflow. The interface is essentially "upload and
forget", which has a significant positive impact on the conversion
funnel of Python packages.

Flakes are easy to install, but the publishing workflow is not yet polished
enough. The current approach to distributing flakes appears to have
inherinted many characteristics of the Nixpkgs workflow.

For Nixpkgs, this was the natural way of development, because all
derivations form a large and coupled Nix expression split across many
files within a single Git repository.

## Service
The service is deployed at [a-piece-of-flake](https://pieceofflakenixrepository.org/)

## Development environment

```shell
$ nix develop
$ emacs &
$ cabal build
$ cabal test
```
### Update cachix

``` shell
nix build --no-link --print-out-paths | cachix push piece-of-flake
```

#### Cachix public key

``` shell
piece-of-flake.cachix.org-1:JamWDEABLpvWhGHIK7Xn/qYFbdZqb6ne7IAF62fXyFY=
```

## Release
```shell
$ nix build $(e -static true)
$ ./result/bin/a-piece-of-flake run
```
