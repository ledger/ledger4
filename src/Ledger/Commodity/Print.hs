module Ledger.Commodity.Print where

import           Data.Number.CReal
import qualified Data.IntMap as IntMap
import           Data.Ratio
import           Data.Text
import           Ledger.Amount
import           Ledger.Commodity.Pool
import           Ledger.Types

-- | Render an Amount as a decimal floating-point string, properly rounded.
--
-- >>> renderAmount testPool (Amount (52345 % 1000) 1 Nothing)
-- "$52.35"

renderAmount :: CommodityPool -> Amount -> String
renderAmount _ (Amount xq 0 (Just xp)) =
  showCReal (fromIntegral xp) (fromRational xq)

renderAmount _ (Amount xq 0 Nothing) =
  showCReal (fromIntegral extendByDigits) (fromRational xq)

renderAmount pool (Amount xq xc (Just xp)) =
  let cmdty = pool IntMap.! xc
      name  = unpack (cmdtyName cmdty)
  in name ++ showCReal (fromIntegral xp) (fromRational xq)

renderAmount pool (Amount xq xc Nothing) =
  let cmdty = pool IntMap.! xc
      name  = unpack (cmdtyName cmdty)
      prec  = fromIntegral (cmdtyPrecision cmdty)
  in name ++ showCReal prec (fromRational xq)
