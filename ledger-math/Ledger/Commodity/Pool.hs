{-# LANGUAGE OverloadedStrings #-}

module Ledger.Commodity.Pool where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Ledger.Types

type CommodityPool = IntMap Commodity

dollars :: Commodity
dollars = Commodity { cmdtySymbol       = "$"
                    , cmdtyPrecision    = 2
                    , cmdtySuffixed     = False
                    , cmdtySeparated    = False
                    , cmdtyThousands    = True
                    , cmdtyDecimalComma = False
                    , cmdtyNoMarket     = True
                    , cmdtyBuiltin      = False
                    , cmdtyKnown        = True
                    , cmdtyPrimary      = True }

testPool :: CommodityPool
testPool = IntMap.fromList [(1, dollars)]