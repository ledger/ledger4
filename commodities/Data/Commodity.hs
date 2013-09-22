{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Commodity where

import Control.Applicative
import Control.Comonad.Trans.Store
import Control.Lens
import Data.Data
import Data.Foldable as Foldable hiding (toList, foldl')
import Data.Functor.Bind as Bind
import Data.IntMap as IntMap
import Data.Key hiding (adjust, lookup, foldlWithKey')
import Data.Monoid
import Data.Traversable as Traversable
import Linear.Vector
import Prelude hiding (lookup)

type Commodity = IntMap.Key

noCommodity :: Commodity
noCommodity = 0

-- | A value representing either zero (all zeroes are equivalent), a
-- commoditized value, or a vector space of values indexed by commodity.
data Balance a = Zero
               | Plain a            -- ^ An uncommoditized integer
               | Amount Commodity a -- ^ A single commoditized amount
               | Balance (IntMap a) -- ^ A vector-space over commodities
               deriving (Show, Read, Typeable, Data)

instance Eq (Balance a)

non' :: (Functor f, Profunctor p) => a -> p a (f a) -> p (Maybe a) (f (Maybe a))
non' = anon ?? const False

instance Additive Balance where
    zero = Zero

    Zero ^+^ x = x
    x ^+^ Zero = x

    Plain qx ^+^ Plain qy      = Plain (qx + qy)
    Plain qx ^+^ Amount cy qy  = Amount cy (qx + qy)
    Amount cx qx ^+^ Plain qy  = Amount cx (qx + qy)
    Plain qx ^+^ y@(Balance _) = Amount noCommodity qx ^+^ y
    x@(Balance _) ^+^ Plain qy = x ^+^ Amount noCommodity qy

    Amount cx qx ^+^ Amount cy qy
        | cx == cy  = Amount cx (qx + qy)
        | otherwise = Balance $ fromList [(cx,qx), (cy,qy)]
    Amount cx qx ^+^ Balance ys = Balance $ ys & at cx.non' 0 +~ qx
    Balance xs ^+^ Amount cy qy = Balance $ xs & at cy.non' 0 +~ qy

    Balance xs ^+^ Balance ys = Balance $ xs ^+^ ys
    {-# INLINE (^+^) #-}

    Zero ^-^ x    = fmap negate x
    x    ^-^ Zero = x
    x    ^-^ y    = x ^+^ (Zero ^-^ y)
    {-# INLINE (^-^) #-}

instance Functor Balance where
    fmap _ Zero         = Zero
    fmap f (Plain x)    = Plain (f x)
    fmap f (Amount c x) = Amount c (f x)
    fmap f (Balance xs) = Balance $ fmap f xs

instance Applicative Balance where
    pure = Plain

    Zero <*> _ = Zero
    _ <*> Zero = Zero

    Plain f <*> Plain qy     = Plain (f qy)
    Plain f <*> Amount cy qy = Amount cy (f qy)
    Amount cx f <*> Plain qy = Amount cx (f qy)
    Plain f <*> Balance xs   = Balance $ fmap f xs
    Balance fs <*> Plain qy  = Balance $ fmap ($ qy) fs

    Amount cx f <*> Amount cy qy
        | cx == cy = Amount cy (f qy)
        | otherwise = Zero

    Amount cx f <*> Balance xs =
        maybe Zero (Amount cx . f) $ lookup cx xs
    Balance fs <*> Amount cy qy =
        maybe Zero (\f -> Amount cy (f qy)) $ lookup cy fs

    Balance fs <*> Balance ys =
        Balance $ intersectionWith ($) fs ys

instance Apply Balance where
    (<.>) = (<*>)

instance Bind Balance where
    Zero >>- _       = Zero
    Plain q >>- f    = f q
    Amount _ q >>- f = f q
    Balance xs >>- f = foldlWithKey' (\acc k x -> f x ^+^ acc) Zero xs

instance Monad Balance where
    return = pure
    (>>=) = (>>-)

type instance Data.Key.Key Balance = IntMap.Key

-- instance Lookup Balance where
--     lookup _ Zero = Nothing
--     lookup k (Amount c x) = if k == c then Just x else Nothing
--     lookup k (Balance xs) = case IntMap.lookup k xs of
--         Nothing -> Nothing
--         Just x  -> Data.Key.lookup k x

-- instance Data.Key.Indexable Balance where
--     index Zero _ = error "Key not in zero Balance"
--     index (Amount c x) k = if c == k
--                            then x
--                            else error "Key not in zero Balance"
--     index (Balance xs) k = Data.Key.index (Data.Key.index xs k) k

-- -- instance At (Balance a) where
-- --   at f k Zero         = const Zero
-- --   at k f (Amount c q) = undefined
-- --   at k f (Balance xs) = undefined -- at k (fmap f) xs

-- instance Adjustable Balance where
--     adjust _ _ Zero         = Zero
--     adjust f _ (Amount c q) = Amount c (f q)
--     adjust f k (Balance xs) = Balance (IntMap.adjust (fmap f) k xs)

-- instance Foldable Balance where
--     foldMap _ Zero         = mempty
--     foldMap f (Amount _ x) = f x
--     foldMap f (Balance xs) = foldMap (foldMap f) xs

--     foldr _ z Zero         = z
--     foldr f z (Amount _ x) = f x z
--     foldr f z (Balance xs) = Foldable.foldr (flip (Foldable.foldr f)) z xs

-- instance Traversable Balance where
--     traverse _ Zero         = pure Zero
--     traverse f (Amount c x) = fmap (Amount c) (f x)
--     traverse f (Balance xs) = fmap Balance (traverse (traverse f) xs)

--     sequenceA Zero         = pure Zero
--     sequenceA (Amount c x) = fmap (Amount c) x
--     sequenceA (Balance xs) = fmap Balance (traverse sequenceA xs)

-- instance Num a => Monoid (Balance a) where
--     mempty = Zero

--     mappend Zero x = x
--     mappend y Zero = y

--     mappend x@(Amount cx qx) y@(Amount cy qy)
--         | cx /= cy   = Balance (IntMap.fromList [(cx,x),(cy,y)])
--         | otherwise = Amount cx (qx + qy)

--     mappend x@(Amount cx _) (Balance ys) =
--         Balance (IntMap.singleton cx x <> ys)
--     mappend (Balance xs) y@(Amount cy _) =
--         Balance (xs <> IntMap.singleton cy y)

--     mappend (Balance xs) (Balance ys) = Balance (xs <> ys)

-- class Monoid g => Group g where
--     inverse :: g -> g

-- instance Num a => Group (Balance a) where
--     inverse x = Zero ^-^ x

-- balanceStore :: Data.Key.Indexable f
--              => Data.Key.Key f -> f a -> Store (Data.Key.Key f) a
-- balanceStore k x = store (Data.Key.index x) k

-- sum :: Num a => [Balance a] -> Balance a
-- sum = Foldable.foldr (^+^) Zero
