{ mkDerivation, aeson, base, bytestring, containers, hpack, hspec
, hspec-wai, hspec-wai-json, HTTP, http-client, http-client-tls
, http-media, lens, lib, lucid, raw-strings-qq, relude, servant
, servant-client, servant-server, text, time, vector, wai, warp
, executableSystemDepends
, pkgs ? import <nixpkgs> {},
}:
mkDerivation {
  inherit executableSystemDepends;
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers HTTP http-client http-client-tls
    http-media lens lucid raw-strings-qq relude servant servant-client
    servant-server text time vector wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers HTTP http-client http-client-tls
    http-media lens lucid raw-strings-qq relude servant servant-client
    servant-server text time vector wai warp
  ];
  testHaskellDepends = [
    aeson base bytestring containers hspec hspec-wai hspec-wai-json
    HTTP http-client http-client-tls http-media lens lucid
    raw-strings-qq relude servant servant-client servant-server text
    time vector wai warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/LATBauerdick/backend#readme";
  description = "backend for Clutter";
  license = lib.licenses.bsd3;
}
