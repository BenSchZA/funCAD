{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "cadcad-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
