module Ledger.Utils where

maybeEqual :: Eq a => Maybe a -> Maybe a -> Bool
maybeEqual _ Nothing = True
maybeEqual Nothing _ = True
maybeEqual (Just x) (Just y) = x == y

-- Utils.hs ends here
