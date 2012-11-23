module Ledger.Types where

import Data.Ratio
import Data.Text

data Commodity = Commodity { cmdtySymbol       :: Text
                           , cmdtyPrecision    :: Int
                           , cmdtySuffixed     :: Bool
                           , cmdtySeparated    :: Bool
                           , cmdtyThousands    :: Bool
                           , cmdtyDecimalComma :: Bool
                           , cmdtyNoMarket     :: Bool
                           , cmdtyBuiltin      :: Bool
                           , cmdtyKnown        :: Bool
                           , cmdtyPrimary      :: Bool }
               deriving (Show, Read, Eq)

data Amount = Amount { amtQuantity      :: Ratio Integer
                     , amtCommodity     :: Int
                     , amtPrecision     :: Maybe Int }
            deriving (Show, Read, Eq)

-- Types.hs ends here
