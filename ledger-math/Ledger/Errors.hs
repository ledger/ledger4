{-# LANGUAGE DeriveDataTypeable #-}

module Ledger.Errors where

import Control.Exception
import Data.Text
import Data.Typeable
import Ledger.Types

data LedgerException = CommodityMismatch { leCmOperator :: Text
                                         , leCmLeft     :: Amount
                                         , leCmRight    :: Amount }
                     deriving (Show, Typeable)

instance Exception LedgerException
