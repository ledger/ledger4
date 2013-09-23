module Data.Commodity.Utils where

maybeEqual :: Eq a => Maybe a -> Maybe a -> Bool
maybeEqual _ Nothing = True
maybeEqual Nothing _ = True
maybeEqual (Just x) (Just y) = x == y

maybeEqualBy :: Eq b => (a -> b) -> Maybe a -> Maybe a -> Bool
maybeEqualBy _ _ Nothing = True
maybeEqualBy _ Nothing _ = True
maybeEqualBy f (Just x) (Just y) = f x == f y

-- Utils.hs ends here
