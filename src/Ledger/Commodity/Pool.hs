module Ledger.Commodity.Pool where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Text
import           Ledger.Types

type CommodityPool = IntMap Commodity

testPool :: CommodityPool
testPool = IntMap.fromList [(1, Commodity (pack "$") 2)]