module Ledger.Commodity.Parse where

import Control.Monad.Trans.State
import Data.Text
import Ledger.Balance
import Ledger.Commodity

parseAmount :: Text -> State CommodityMap (Balance a)
parseAmount _str = undefined
