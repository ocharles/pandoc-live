{ cabal
, async, linuxInotify, pandoc, snap, text
}:
cabal.mkDerivation (self: {
  pname = "pandoc-live";
  src = ./.;
  version = "1.0";
  buildDepends = [ async linuxInotify pandoc snap text ];
})