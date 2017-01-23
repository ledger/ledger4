{ mkDerivation, base, comonad, containers, directory, distributive
, doctest, failure, filepath, hspec, hspec-expectations, keys, lens
, linear, mtl, numbers, parsers, PSQueue, QuickCheck, semigroupoids
, semigroups, split, stdenv, text, thyme, transformers, trifecta
}:
mkDerivation {
  pname = "commodities";
  version = "0.2.0";
  src = ./.;
  libraryHaskellDepends = [
    base comonad containers distributive failure keys lens linear mtl
    numbers parsers PSQueue semigroupoids semigroups split text thyme
    transformers trifecta
  ];
  testHaskellDepends = [
    base containers directory doctest filepath hspec hspec-expectations
    lens QuickCheck semigroups thyme transformers
  ];
  description = "Library for working with commoditized amounts and price histories";
  license = stdenv.lib.licenses.bsd3;
}
