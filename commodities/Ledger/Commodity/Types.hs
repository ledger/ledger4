{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Commodity.Types where

import Control.Applicative
import Control.Comonad.Trans.Store
import Control.Lens
import Data.Data
import Data.Foldable as Foldable
import Data.Functor.Bind as Bind
import Data.IntMap as IntMap
import Data.Key
import Data.Monoid
import Data.Traversable as Traversable
import Linear.Vector

nullCommodity :: Int
nullCommodity = 0

type CommodityIdx = IntMap.Key

data Balance a = Zero
               | Amount CommodityIdx a
               | Balance (IntMap (Balance a))
               deriving (Typeable, Data)

instance Eq (Balance a)
instance Show (Balance a)
instance Read (Balance a)

instance Additive Balance where
    zero = Zero

    Zero ^+^ x    = x
    x    ^+^ Zero = x

    x@(Amount cx qx) ^+^ y@(Amount cy qy)
        | cx /= cy   = Balance (fromList [(cx,x), (cy,y)])
        | otherwise = Amount cx (qx + qy)

    x@(Amount cx _) ^+^ Balance ys =
        Balance (ys & at cx.non Zero %~ (^+^ x))
    Balance xs ^+^ y@(Amount cy _) =
        Balance (xs & at cy.non Zero %~ (^+^ y))

    x@(Balance _) ^+^ Balance ys = Balance (fmap (x ^+^) ys)
    {-# INLINE (^+^) #-}

    Zero ^-^ Zero       = Zero
    Zero ^-^ Amount c q = Amount c (-q)
    Zero ^-^ Balance ys = Balance (fmap (Zero ^-^) ys)
    x    ^-^ Zero       = x

    x@(Amount cx qx) ^-^ y@(Amount cy qy)
        | cx /= cy   = Balance (fromList [(cx,x), (cy,Zero ^-^ y)])
        | otherwise = Amount cx (qx - qy)

    x@(Amount cx _) ^-^ Balance ys =
        Balance (fmap (Zero ^-^) ys & at cx.non Zero %~ (^+^ x))
    Balance xs ^-^ y@(Amount cy _) =
        Balance (xs & at cy.non Zero %~ (^-^ y))

    x@(Balance _) ^-^ Balance ys = Balance (fmap (x ^-^) ys)
    {-# INLINE (^-^) #-}

instance Functor Balance where
    fmap _ Zero         = Zero
    fmap f (Amount c x) = Amount c (f x)
    fmap f (Balance xs) = Balance (fmap (fmap f) xs)

instance Applicative Balance where
    pure = Amount nullCommodity

    Zero <*> _ = Zero

    Amount _ _ <*> Zero         = Zero
    Amount _ f <*> Amount cy qy = Amount cy (f qy)
    Amount _ f <*> Balance xs   = Balance (fmap (fmap f) xs)

    Balance _  <*> Zero           = Zero
    Balance fs <*> y@(Amount _ _) = Balance (fmap (<*> y) fs)
    Balance fs <*> Balance ys     =
        Balance $ fmap (\y -> Balance (fmap (<*> y) fs)) ys

instance Apply Balance where
    (<.>) = (<*>)

instance Bind Balance where
    Zero >>- _       = Zero
    Amount c q >>- f = case f q of
        Zero        -> Zero
        Amount _ q' -> Amount c q'
        Balance xs  -> xs^.at c.non Zero
    Balance xs >>- f = Balance . IntMap.filter (/= Zero) . fmap (>>- f) $ xs

instance Monad Balance where
    return = pure
    (>>=) = (>>-)

type instance Data.Key.Key Balance = IntMap.Key

instance Lookup Balance where
    lookup _ Zero = Nothing
    lookup k (Amount c x) = if k == c then Just x else Nothing
    lookup k (Balance xs) = case IntMap.lookup k xs of
        Nothing -> Nothing
        Just x  -> Data.Key.lookup k x

instance Data.Key.Indexable Balance where
    index Zero _ = error "Key not in zero Balance"
    index (Amount c x) k = if c == k
                           then x
                           else error "Key not in zero Balance"
    index (Balance xs) k = Data.Key.index (Data.Key.index xs k) k

-- instance At (Balance a) where
--   at f k Zero         = const Zero
--   at k f (Amount c q) = undefined
--   at k f (Balance xs) = undefined -- at k (fmap f) xs

instance Adjustable Balance where
    adjust _ _ Zero         = Zero
    adjust f _ (Amount c q) = Amount c (f q)
    adjust f k (Balance xs) = Balance (IntMap.adjust (fmap f) k xs)

instance Foldable Balance where
    foldMap _ Zero         = mempty
    foldMap f (Amount _ x) = f x
    foldMap f (Balance xs) = foldMap (foldMap f) xs

    foldr _ z Zero         = z
    foldr f z (Amount _ x) = f x z
    foldr f z (Balance xs) = Foldable.foldr (flip (Foldable.foldr f)) z xs

instance Traversable Balance where
    traverse _ Zero         = pure Zero
    traverse f (Amount c x) = fmap (Amount c) (f x)
    traverse f (Balance xs) = fmap Balance (traverse (traverse f) xs)

    sequenceA Zero         = pure Zero
    sequenceA (Amount c x) = fmap (Amount c) x
    sequenceA (Balance xs) = fmap Balance (traverse sequenceA xs)

instance Num a => Monoid (Balance a) where
    mempty = Zero

    mappend Zero x = x
    mappend y Zero = y

    mappend x@(Amount cx qx) y@(Amount cy qy)
        | cx /= cy   = Balance (IntMap.fromList [(cx,x),(cy,y)])
        | otherwise = Amount cx (qx + qy)

    mappend x@(Amount cx _) (Balance ys) =
        Balance (IntMap.singleton cx x <> ys)
    mappend (Balance xs) y@(Amount cy _) =
        Balance (xs <> IntMap.singleton cy y)

    mappend (Balance xs) (Balance ys) = Balance (xs <> ys)

class Monoid g => Group g where
    inverse :: g -> g

instance Num a => Group (Balance a) where
    inverse x = Zero ^-^ x

balanceStore :: Data.Key.Indexable f
             => Data.Key.Key f -> f a -> Store (Data.Key.Key f) a
balanceStore k x = store (Data.Key.index x) k

sum :: Num a => [Balance a] -> Balance a
sum = Foldable.foldr (^+^) Zero
