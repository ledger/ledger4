module Ledger.Types where

import Data.Ratio
import Data.Text

data Commodity = Commodity { cmdtyName      :: Text
                           , cmdtyPrecision :: Int }
               deriving (Show, Read, Eq)

data Amount = Amount { amtQuantity      :: Ratio Integer
                     , amtCommodity     :: Int
                     , amtPrecision     :: Maybe Int }
            deriving (Show, Read, Eq)

-- Types.hs ends here
