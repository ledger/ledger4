{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Commodity.Print
    ( printBalance
    , balance
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import "mtl"     Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader (runReader)
import           Control.Monad.Trans.State (evalState)
import           Control.Monad.Trans.Writer
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Data.Maybe (fromMaybe)
import           Data.Number.CReal
import           Data.Text.Lazy (Text, pack)
--import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder
import           Ledger.Balance
import           Ledger.Commodity
import           Ledger.Commodity.Parse

printBalance :: (MonadReader CommodityMap m, Functor m, a ~ Rational)
             => Balance a
             -> m Text
printBalance Zero      = return "0"
printBalance (Plain x) = return $ pack (show x)
printBalance x         = toLazyText <$> execWriterT (buildBalance x)

buildBalance :: (MonadReader CommodityMap m, Functor m, a ~ Rational)
             => Balance a
             -> WriterT Builder m ()
buildBalance (Amount c q) = do
    cm <- fromMaybe defaultCommodityInfo <$> view (commodities.at c)

    unless (cm^.commSuffixed) $ do
        outputSymbol cm
        when (cm^.commSeparated) $
            tell $ fromLazyText " "

    tell $ fromString (formatAmount cm)

    when (cm^.commSuffixed) $ do
        when (cm^.commSeparated) $
            tell $ fromLazyText " "
        outputSymbol cm
  where
    outputSymbol cm = tell $ fromText (cm^.commSymbol)

    formatAmount cm =
        let prec   = cm^.commPrecision
            str    = showCReal prec (fromRational q)
            (n, m) = case break (== '.') str of
                (xs, '.':ys) -> (xs, ys)
                (xs, ys)     -> (xs, ys)
            len    = length m
            (com, per) = if cm^.commDecimalComma
                         then (".", ",")
                         else (",", ".")
            n' = if cm^.commThousands
                 then reverse . intercalate com . chunksOf 3 . reverse $ n
                 else n
            m' = if len < prec
                 then m ++ replicate (prec - len) '0'
                 else m
        in intercalate per [n', m']

buildBalance (Balance xs) =
    mapM_ (buildBalance . uncurry Amount) $ IntMap.toList xs

buildBalance _ = return ()

balance :: a ~ Rational => CommodityMap -> Iso' (Balance a) Text
balance pool = iso toBalance fromBalance
  where
    toBalance   = flip runReader pool . printBalance
    fromBalance = flip evalState pool . parseBalance
