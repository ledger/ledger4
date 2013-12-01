module Ledger.Commodity.Print where

import Control.Monad.Trans.State
import Data.Text
import Ledger.Balance
import Ledger.Commodity

printAmount :: Balance a -> State CommodityMap Text
printAmount _x = undefined
