{ mkDerivation, aeson, async, base, bytestring, direct-sqlite
, http-conduit, lens, lens-aeson, lib, random, silently
, sqlite-simple, text, time
}:
mkDerivation {
  pname = "fvckbot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring direct-sqlite http-conduit lens
    lens-aeson random silently sqlite-simple text time
  ];
  homepage = "https://github.com/nicball/fvckbot#readme";
  license = lib.licenses.bsd3;
  mainProgram = "fvckbot";
}
