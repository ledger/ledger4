{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Commodity.Amount where

import Control.Applicative
import Data.Ratio
import Data.Commodity.Types

extendByDigits :: Int
extendByDigits = 6

-- instance Ord Amount where
--   x@(Amount xq _ _) <= y@(Amount yq _ _) =
--     ifCommoditiesMatch "<=" x y (xq <= yq)

-- instance Real Amount where
--   toRational = amtQuantity

-- instance Num Amount where
--   abs    = overAmount abs
--   signum = overAmount signum

--   fromInteger x = basic (fromInteger x)

--   x@(Amount xq xc xp) + y@(Amount yq _ yp) =
--     ifCommoditiesMatch "+" x y $ Amount (xq + yq) xc (liftA2 max xp yp)

--   Amount xq xc xp * Amount yq _ yp = Amount (xq * yq) xc (liftA2 (+) xp yp)

-- instance Fractional Amount where
--   fromRational x = basic (fromRational x)

--   Amount xq xc xp / Amount yq _ yp =
--     Amount (xq * yq) xc (liftA2 (+) (liftA2 (+) xp yp) (Just extendByDigits))

-- overAmount :: (Ratio Integer -> Ratio Integer) -> Amount -> Amount
-- overAmount f x = x { amtQuantity = f (amtQuantity x) }

-- basic :: Ratio Integer -> Amount
-- basic x = Amount { amtQuantity      = x
--                  , amtCommodity     = 0
--                  , amtPrecision     = Just extendByDigits }

-- zero :: Amount
-- zero = basic 0

-- Amount.hs ends here
