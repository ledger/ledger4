module Ledger.Amount where

import Control.Exception.Base
import Data.Ratio
import Ledger.Commodity

data Amount = Amount { amtCommodity :: Maybe Commodity
                     , amtQuantity  :: Ratio Integer
                     , amtPrecision :: Int }
              deriving (Eq, Read, Show)

instance Num Amount where
  Amount xc xq _ + Amount yc yq _ =
    assert (commoditiesMatch xc yc) (Amount xc (xq + yq) 0)

  Amount xc xq _ * Amount _ yq _ = Amount xc (xq * yq) 0

  abs (Amount xc xq _)    = Amount xc (abs xq) 0
  signum (Amount xc xq _) = Amount xc (signum xq) 0
  fromInteger x           = Amount Nothing (fromInteger x) 0

instance Ord Amount where
  Amount xc xq _ <= Amount yc yq _ =
    assert (commoditiesMatch xc yc) (xq <= yq)

zero :: Amount
zero = Amount Nothing 0 0