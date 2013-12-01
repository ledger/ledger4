{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

module Ledger.Commodity.Parse where

import "mtl" Control.Monad.State.Class
import       Data.Text.Lazy
import       Ledger.Balance
import       Ledger.Commodity

parseBalance :: MonadState CommodityMap m => Text -> m (Balance a)
parseBalance _str = undefined
