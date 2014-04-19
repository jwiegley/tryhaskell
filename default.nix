{ cabal, parallelIo, shake, filepath, text, temporary
}:

cabal.mkDerivation (self: {
  pname = "tryhaskell";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ parallelIo shake filepath text temporary ];
  meta = {
    homepage = "https://github.com/jwiegley/tryhaskell";
    description = "Process for building the Try Haskell! app bundle";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
