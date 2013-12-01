{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ledger.Commodity
       ( Commodity
       , CommodityInfo(..)
       , defaultCommodityInfo
       , CommodityMap(..)
       ) where

import           Data.IntMap (IntMap, Key)
import qualified Data.IntMap as IntMap
import           Data.Map (Map)
import           Data.Ratio
import           Data.Semigroup
import           Data.Text (Text)
import           Data.Thyme.Time
import           Prelude hiding (lookup)

-- | Commodities are simply indices into a commodity info map, which relates
--   such commodities to the information known about them.
type Commodity = Key

-- | All of the information known about a commodity.
data CommodityInfo = CommodityInfo
    { commSymbol       :: !Text
    , commPrecision    :: !Int
    , commSuffixed     :: !Bool
    , commSeparated    :: !Bool
    , commThousands    :: !Bool
    , commDecimalComma :: !Bool
    , commNoMarket     :: !Bool
    , commBuiltin      :: !Bool
    , commKnown        :: !Bool
    , commPrimary      :: !Bool
    , commHistory      :: !(IntMap (Map UTCTime Rational))
    } deriving (Eq, Read, Show)

-- | Return a 'CommodityInfo' with defaults selected for all fields.  It is
--   intended that at least one field of the result will be modified
--   immediately.
defaultCommodityInfo :: CommodityInfo
defaultCommodityInfo = CommodityInfo
    { commSymbol       = ""
    , commPrecision    = 0
    , commSuffixed     = False
    , commSeparated    = False
    , commThousands    = True
    , commDecimalComma = False
    , commNoMarket     = False
    , commBuiltin      = False
    , commKnown        = False
    , commPrimary      = False
    , commHistory      = IntMap.empty
    }

-- | A commodities map, relating commodity indices to information about
--   those commodities.
data CommodityMap = CommodityMap
    { commodities :: IntMap CommodityInfo
    }
    deriving (Eq, Read, Show)

instance Semigroup CommodityMap where
    CommodityMap x <> CommodityMap y = CommodityMap (x <> y)

instance Monoid CommodityMap where
    mempty = CommodityMap mempty
    x `mappend` y = x <> y
