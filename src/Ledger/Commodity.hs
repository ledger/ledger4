module Ledger.Commodity where

import Data.Maybe
import Data.Text
import Ledger.Utils

type Commodity = Text

commoditiesMatch :: Maybe Text -> Maybe Text -> Bool
commoditiesMatch x y
  | maybeEqual x y = True
  | otherwise =
    error $ "Commodities do not match: "
         ++ unpack (fromJust x) ++ " != " ++ unpack (fromJust y)
