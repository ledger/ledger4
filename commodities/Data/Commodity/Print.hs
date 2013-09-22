{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Commodity.Print where

import           Data.Number.CReal
import qualified Data.IntMap as IntMap
import           Data.Ratio
import           Data.Text
import           Data.Commodity.Amount
import           Data.Commodity.Pool
import           Data.Commodity.Types

-- | Render an Amount as a decimal floating-point string, properly rounded.
--
-- jww (2012-11-23): This code currently uses 'Data.Number.CReal', but the
-- intention is to move to edwardk's rounded library, which is a wrapper
-- around MPFR.
--
-- >>> renderAmount testPool (Amount (5234567 % 100000) 1 Nothing)
-- "$52.35"
-- >>> renderAmount testPool (Amount (5234567 % 100000) 1 (Just 3))
-- "$52.346"
-- >>> renderAmount testPool (Amount (5234567 % 100000) 0 Nothing)
-- "52.34567"
-- >>> renderAmount testPool (Amount (5234567 % 100000) 0 (Just 3))
-- "52.346"

-- renderAmount :: CommodityPool -> Amount -> String
-- renderAmount _ (Amount xq 0 (Just xp)) =
--   showCReal (fromIntegral xp) (fromRational xq)

-- renderAmount _ (Amount xq 0 Nothing) =
--   showCReal (fromIntegral extendByDigits) (fromRational xq)

-- renderAmount pool (Amount xq xc (Just xp)) =
--   let cmdty = pool IntMap.! xc
--       name  = unpack (cmdtySymbol cmdty)
--   in name ++ showCReal (fromIntegral xp) (fromRational xq)

-- renderAmount pool (Amount xq xc Nothing) =
--   let cmdty = pool IntMap.! xc
--       name  = unpack (cmdtySymbol cmdty)
--       prec  = fromIntegral (cmdtyPrecision cmdty)
--   in name ++ showCReal prec (fromRational xq)

-- Print.hs ends here