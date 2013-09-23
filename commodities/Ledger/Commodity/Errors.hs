{-# LANGUAGE DeriveDataTypeable #-}

module Data.Commodity.Errors where

import Control.Exception
import Data.Text
import Data.Typeable
import Data.Commodity.Types

-- data CommodityException = CommodityMismatch
--     { leCmOperator :: Text
--     , leCmLeft     :: Amount
--     , leCmRight    :: Amount
--     } deriving (Show, Typeable)

-- instance Exception CommodityException
