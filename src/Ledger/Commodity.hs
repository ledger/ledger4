module Ledger.Commodity where

import Control.Exception
import Data.Text
import Ledger.Errors
import Ledger.Types

ifCommoditiesMatch :: Text -> Amount -> Amount -> a -> a
ifCommoditiesMatch op x@(Amount _ xc _) y@(Amount _ yc _) z
  | xc == 0 || yc == 0 || xc == yc = z
  | otherwise = throw (CommodityMismatch op x y)

-- Commodity.hs ends here
