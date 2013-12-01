{-# LANGUAGE FlexibleContexts #-}

module Ledger.Commodity.Print where

import Control.Monad.Reader.Class
import Data.Text
import Data.Text.Lazy.Builder
import Ledger.Balance
import Ledger.Commodity

printAmount :: MonadReader CommodityMap m => Balance a -> m Text
printAmount _x = undefined
