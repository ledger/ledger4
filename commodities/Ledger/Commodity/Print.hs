{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Ledger.Commodity.Print
    ( printBalance
    , balance
    ) where

import           Control.Applicative
import           Control.Lens
import "mtl"     Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader (runReader)
import           Control.Monad.Trans.State (evalState)
import           Control.Monad.Trans.Writer
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe (fromMaybe)
import           Data.Text.Lazy
import           Data.Text.Lazy.Builder
import           Ledger.Balance
import           Ledger.Commodity
import           Ledger.Commodity.Parse

printBalance :: (MonadReader CommodityMap m, Functor m, Show a)
             => Balance a
             -> m Text
printBalance Zero = return "0"
printBalance (Plain x) = return $ pack (show x)
printBalance x = toLazyText <$> execWriterT (buildBalance x)

buildBalance :: (MonadReader CommodityMap m, Show a)
             => Balance a
             -> WriterT Builder m ()
buildBalance (Amount c q) = do
    mcomm <- view (commodities.at c)
    let comm = fromMaybe defaultCommodityInfo mcomm
    tell $ fromText (comm^.commSymbol)
    tell $ fromString (show q)

buildBalance (Balance xs) =
    mapM_ (buildBalance . uncurry Amount) $ IntMap.toList xs

buildBalance _ = return ()

balance :: Show a => CommodityMap -> Iso' (Balance a) Text
balance pool = iso toBalance fromBalance
  where
    toBalance   = flip runReader pool . printBalance
    fromBalance = flip evalState pool . parseBalance
