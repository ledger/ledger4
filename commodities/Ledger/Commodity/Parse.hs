{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Commodity.Parse where

import "mtl"     Control.Monad.State.Class
import           Data.Semigroup
import           Data.Text.Lazy (Text, unpack)
--import qualified Data.Text.Lazy as TL
import           Ledger.Balance
import           Ledger.Commodity
--import           Text.Parser.Char
--import           Text.Parser.Combinators
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

parseBalance :: (MonadState CommodityMap m, a ~ Rational)
             => Text -> m (Balance a)
parseBalance str =
    case parseString balanceParser mempty (unpack str) of
        Success b -> return b
        Failure _e -> undefined

balanceParser :: a ~ Rational => Parser (Balance a)
balanceParser = undefined
