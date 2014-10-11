let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      pandocLive = self.callPackage ./. {};
       };
       };

       in pkgs.lib.overrideDerivation haskellPackages.pandocLive (attrs: {
       buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
       })