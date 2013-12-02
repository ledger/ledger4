{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Commodity.Parse where

import           Control.Applicative
import           Control.Lens
import "mtl"     Control.Monad.State.Class
import qualified Data.IntMap.Strict as IntMap
import           Data.Semigroup
import qualified Data.Text as T
import           Data.Text.Lazy (Text, unpack)
--import qualified Data.Text.Lazy as TL
import           Ledger.Balance
import           Ledger.Commodity
--import           Text.Parser.Char
--import           Text.Parser.Combinators
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

parseBalance :: (MonadState CommodityMap m, Functor m, a ~ Rational)
             => Text -> m (Either BalanceError (Balance a))
parseBalance str =
    case parseString balanceParser mempty (unpack str) of
        Success (b, c) -> do
            len <- IntMap.size <$> use commodities
            commodities.at len .= Just c
            return . Right $ b
        Failure e ->
            return . Left $ BalanceParseError (T.pack (show e))

balanceParser :: a ~ Rational => Parser (Balance a, CommodityInfo)
balanceParser = undefined
