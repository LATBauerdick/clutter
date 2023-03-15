{ mkDerivation, aeson, base, bytestring, containers, hashable
, hspec, hspec-wai, hspec-wai-json, HTTP, http-client
, http-client-tls, http-media, lib, lucid, monad-loops
, raw-strings-qq, relude, servant, servant-client, servant-server
, text, time, vector, wai, warp
, executableSystemDepends
, pkgs ? import <nixpkgs> {},
}:
mkDerivation {
  inherit executableSystemDepends;
  pname = "clutter";
  version = "0.1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers hashable HTTP http-client
    http-client-tls http-media lucid monad-loops raw-strings-qq relude
    servant servant-client servant-server text time vector wai warp
  ];
  # libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers hashable HTTP http-client
    http-client-tls http-media lucid monad-loops raw-strings-qq relude
    servant servant-client servant-server text time vector wai warp
  ];
  testHaskellDepends = [
    aeson base bytestring containers hashable hspec hspec-wai
    hspec-wai-json HTTP http-client http-client-tls http-media lucid
    monad-loops raw-strings-qq relude servant servant-client
    servant-server text time vector wai warp
  ];
  # prePatch = "hpack";
  homepage = "https://github.com/LATBauerdick/clutter-hs#readme";
  description = "server for Clutter";
  license = lib.licenses.bsd3;
  mainProgram = "clutter-exe";
}
