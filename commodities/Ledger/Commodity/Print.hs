{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ledger.Commodity.Print where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Writer
import qualified Data.IntMap.Strict as IntMap
import           Data.Text.Lazy
import           Data.Text.Lazy.Builder
import           Ledger.Balance
import           Ledger.Commodity

printAmount :: (MonadReader CommodityMap m, Functor m, Show a)
            => Balance a
            -> m Text
printAmount Zero = return "0"
printAmount (Plain x) = return $ pack (show x)
printAmount x = toLazyText <$> execWriterT (buildAmount x)

buildAmount :: (MonadReader CommodityMap m, Show a)
            => Balance a
            -> WriterT Builder m ()
buildAmount (Amount c q) = do
    mcomm <- view (commodities.at c)
    case mcomm of
        Nothing -> error "Attempted to print an unknown commodity"
        Just comm -> do
            tell $ fromText (comm^.commSymbol)
            tell $ fromString (show q)

buildAmount (Balance xs) =
    mapM_ (buildAmount . uncurry Amount) $ IntMap.toList xs

buildAmount _ = return ()
