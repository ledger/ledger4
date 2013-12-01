{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ledger.Commodity
       ( Commodity
       , CommodityInfo(..), HasCommodityInfo(..)
       , defaultCommodityInfo
       , CommodityMap(..), HasCommodityMap(..)
       , extendByDigits
       ) where

import           Control.Lens
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

extendByDigits :: Int
extendByDigits = 6

-- | All of the information known about a commodity.
data CommodityInfo = CommodityInfo
    { _commSymbol       :: !Text
    , _commPrecision    :: !Int
    , _commSuffixed     :: !Bool
    , _commSeparated    :: !Bool
    , _commThousands    :: !Bool
    , _commDecimalComma :: !Bool
    , _commNoMarket     :: !Bool
    , _commBuiltin      :: !Bool
    , _commKnown        :: !Bool
    , _commPrimary      :: !Bool
    , _commHistory      :: !(IntMap (Map UTCTime Rational))
    } deriving (Eq, Read, Show)

makeClassy ''CommodityInfo

-- | Return a 'CommodityInfo' with defaults selected for all fields.  It is
--   intended that at least one field of the result will be modified
--   immediately.
defaultCommodityInfo :: CommodityInfo
defaultCommodityInfo = CommodityInfo
    { _commSymbol       = ""
    , _commPrecision    = 0
    , _commSuffixed     = False
    , _commSeparated    = False
    , _commThousands    = True
    , _commDecimalComma = False
    , _commNoMarket     = False
    , _commBuiltin      = False
    , _commKnown        = False
    , _commPrimary      = False
    , _commHistory      = IntMap.empty
    }

-- | A commodities map, relating commodity indices to information about
--   those commodities.
data CommodityMap = CommodityMap
    { _commodities :: !(IntMap CommodityInfo)
    }
    deriving (Eq, Read, Show)

makeClassy ''CommodityMap

instance Semigroup CommodityMap where
    CommodityMap x <> CommodityMap y = CommodityMap (x <> y)

instance Monoid CommodityMap where
    mempty = CommodityMap mempty
    x `mappend` y = x <> y
