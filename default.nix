let
  sources = import ./nix/sources.nix;
  tooling = import sources.nix-tooling;
  self = {
    haskell = tooling.haskell.ghc884;
  };
in
  self

