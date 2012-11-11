module Ledger.Amount where

import Data.Ratio

import Ledger.Commodity

data Amount = Amount { amtCommodity :: Maybe Commodity
                     , amtQuantity  :: Ratio Integer }
              deriving (Eq, Read, Show)

instance Num Amount where
  -- jww (2012-11-10): NYI
  Amount xc xq + Amount yc yq = Amount xc (xq + yq)
  -- jww (2012-11-10): NYI
  Amount xc xq * Amount yc yq = Amount xc (xq * yq)

  abs (Amount xc xq)    = Amount xc (abs xq)
  signum (Amount xc xq) = Amount xc (signum xq)
  fromInteger x         = Amount Nothing (fromInteger x)
