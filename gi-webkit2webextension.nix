{ mkDerivation, base, bytestring, Cabal, containers, gi-gobject
, gi-gtk, gi-javascriptcore, gi-soup, haskell-gi, haskell-gi-base
, stdenv, text, transformers, webkit2gtk, webkitgtk
}:
mkDerivation {
  pname = "gi-webkit2webextension";
  version = "4.0.6";
  sha256 = "0xhhd55kxjfbqf8jwmfhwlsybzld79brljsa2v457jvmaw371bf3";
  setupHaskellDepends = [ base Cabal haskell-gi ];
  libraryHaskellDepends = [
    base bytestring containers gi-gobject gi-gtk gi-javascriptcore
    gi-soup haskell-gi-base text transformers
  ];
  libraryPkgconfigDepends = [ webkit2gtk webkitgtk ];
  preConfigure = ''export HASKELL_GI_GIR_SEARCH_PATH=${webkitgtk}/share/gir-1.0'';
  homepage = "https://github.com/haskell-gi/haskell-gi";
  description = "WebKit2-WebExtension bindings";
  license = stdenv.lib.licenses.lgpl21;
}
