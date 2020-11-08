ghcide-setup:
	cachix use ghcide-nix
	nix-env -iA ghcide-ghc865 -f https://github.com/cachix/ghcide-nix/tarball/master

init:
	nix-shell --packages ghc cabal-install --run "cabal init"

cabal2nix:
	nix-shell --packages cabal2nix --run "cabal2nix ." > default.nix

build:
	nix-build release.nix
