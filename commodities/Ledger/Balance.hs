{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ledger.Balance where

import           Control.Applicative
import           Control.Comonad.Trans.Store
import           Control.Lens hiding (from, to)
import           Control.Monad hiding (forM)
import           Data.Data
import           Data.Foldable as Foldable
import           Data.Functor.Bind as Bind
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Key as K
import           Data.Monoid
import           Data.Traversable
import           Ledger.Commodity
import           Linear.Vector
import           Prelude hiding (lookup)

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
        | otherwise = Balance $ IntMap.fromList [(cx,qx), (cy,qy)]
    Amount cx qx ^+^ Balance ys = Balance $ ys & at cx.non' 0 +~ qx
    Balance xs ^+^ Amount cy qy = Balance $ xs & at cy.non' 0 +~ qy

    Balance xs ^+^ Balance ys = Balance $ xs ^+^ ys
    {-# INLINE (^+^) #-}

    Balance xs ^-^ Balance ys = Balance $ xs ^-^ ys

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
        maybe Zero (Amount cx . f) $ IntMap.lookup cx xs
    Balance fs <*> Amount cy qy =
        maybe Zero (\f -> Amount cy (f qy)) $ IntMap.lookup cy fs

    Balance fs <*> Balance ys =
        Balance $ IntMap.intersectionWith ($) fs ys

instance Apply Balance where
    (<.>) = (<*>)

type instance K.Key Balance = IntMap.Key

instance K.Lookup Balance where
    lookup _ Zero         = Nothing
    lookup _ (Plain _)    = Nothing
    lookup k (Amount c x) = if k == c then Just x else Nothing
    lookup k (Balance xs) = IntMap.lookup k xs

instance K.Indexable Balance where
    index Zero _         = error "Key not in zero Balance"
    index (Plain _) _    = error "Key not in plain Balance"
    index (Amount c x) k = if c == k
                           then x
                           else error "Key not in zero Balance"
    index (Balance xs) k = K.index xs k

-- instance At (Balance a) where
--   at f k Zero         = const Zero
--   at k f (Amount c q) = undefined
--   at k f (Balance xs) = undefined -- at k (fmap f) xs

instance K.Adjustable Balance where
    adjust _ _ Zero         = Zero
    adjust f _ (Plain q)    = Plain (f q)
    adjust f _ (Amount c q) = Amount c (f q)
    adjust f k (Balance xs) = Balance (IntMap.adjust f k xs)

instance Foldable Balance where
    foldMap _ Zero         = mempty
    foldMap f (Plain x)    = f x
    foldMap f (Amount _ x) = f x
    foldMap f (Balance xs) = foldMap f xs

    foldr _ z Zero         = z
    foldr f z (Plain x)    = f x z
    foldr f z (Amount _ x) = f x z
    foldr f z (Balance xs) = Foldable.foldr f z xs

instance Traversable Balance where
    traverse _ Zero         = pure Zero
    traverse f (Plain x)    = fmap Plain (f x)
    traverse f (Amount c x) = fmap (Amount c) (f x)
    traverse f (Balance xs) = fmap Balance (traverse f xs)

    sequenceA Zero         = pure Zero
    sequenceA (Plain q)    = fmap Plain q
    sequenceA (Amount c x) = fmap (Amount c) x
    sequenceA (Balance xs) = fmap Balance (sequenceA xs)

instance Num a => Monoid (Balance a) where
    mempty = Zero

    mappend Zero x = x
    mappend y Zero = y

    mappend (Plain qx) (Plain qy) = Plain $ qx + qy
    mappend (Plain qx) (Amount cy qy) = Amount cy (qx + qy)
    mappend (Amount cx qx) (Plain qy) = Amount cx (qx + qy)
    mappend (Plain qx) y = Amount noCommodity qx <> y
    mappend x (Plain qy) = x <> Amount noCommodity qy

    mappend (Amount cx qx) (Amount cy qy)
        | cx == cy  = Amount cx (qx + qy)
        | otherwise = Balance (IntMap.fromList [(cx,qx),(cy,qy)])

    mappend (Amount cx qx) (Balance ys) =
        Balance (IntMap.singleton cx qx <> ys)
    mappend (Balance xs) (Amount cy qy) =
        Balance (xs <> IntMap.singleton cy qy)

    mappend (Balance xs) (Balance ys) = Balance (xs <> ys)

class Monoid g => Group g where
    inverse :: g -> g

instance Num a => Group (Balance a) where
    inverse x = Zero ^-^ x

balanceStore :: K.Indexable f
             => K.Key f -> f a -> Store (K.Key f) a
balanceStore k x = store (K.index x) k

sum :: Num a => [Balance a] -> Balance a
sum = Foldable.foldr (^+^) Zero
