{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Balance
    ( Balance(..)
    , noCommodity
    , balanceSum
    , insert
    , delete
    , balanceStore
    ) where

import           Control.Applicative
import           Control.Comonad.Trans.Store
import           Control.Lens hiding (from, to)
import qualified Control.Lens.Internal as Lens
import           Control.Monad hiding (forM)
import           Data.Data
import           Data.Foldable as Foldable
import           Data.Functor.Bind
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Key as K
import           Data.Semigroup
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
               deriving (Eq, Ord, Show, Read, Typeable, Data)

non' :: a -> Iso' (Maybe a) a
non' = flip anon (const False)

-- instance Num a => Num (Balance a) where
--     x + y = x ^+^ y

--     _ * Zero = Zero
--     Zero * _ = Zero
--     x * Plain q = x ^* q
--     Plain q * y = y ^* q
--     x * Amount _ q = x ^* q
--     Amount _ q * y = y ^* q
--     Balance _ * Balance _ = error "Cannot multiply two balances"

--     x - y = x ^-^ y
--     negate = negated
--     abs x = abs <$> x
--     signum = error "signum not supported on Balance values"
--     fromInteger = Plain . fromInteger

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

instance Bind Balance where
    Zero >>- _    = Zero
    Plain q >>- f = f q

    Amount c q >>- f = case f q of
        Zero    -> Zero
        Plain _ -> Zero
        amt@(Amount c' _)
            | c == c'   -> amt
            | otherwise -> Zero
        Balance xs -> case IntMap.lookup c xs of
            Nothing -> Zero
            Just v  -> Amount c v

    Balance xs >>- f =
        Balance $ IntMap.foldlWithKey' go IntMap.empty xs
      where
        go m c a = case f a of
            Zero    -> m
            Plain _ -> m
            Amount c' q'
                | c == c'   -> IntMap.insert c q' m
                | otherwise -> m
            Balance xs' -> case IntMap.lookup c xs' of
                Nothing -> m
                Just v  -> IntMap.insert c v m

instance Monad Balance where
    return = Plain
    (>>=)  = (>>-)

type instance K.Key Balance = IntMap.Key

instance K.Lookup Balance where
    lookup _ Zero         = Nothing
    lookup _ (Plain _)    = Nothing
    lookup k (Amount c x) = if k == c then Just x else Nothing
    lookup k (Balance xs) = IntMap.lookup k xs

insert :: Int -> a -> Balance a -> Balance a
insert k q Zero = Amount k q
insert k q (Plain q') =
    Balance $ IntMap.fromList [ (noCommodity, q'), (k, q) ]
insert k q (Amount c q') =
    Balance $ IntMap.fromList [ (c, q'), (k, q) ]
insert k q (Balance xs) = Balance $ IntMap.insert k q xs

delete :: Int -> Balance a -> Balance a
delete _k Zero = Zero
delete _k pl@(Plain _) = pl
delete k amt@(Amount c _)
    | k == c = Zero
    | otherwise = amt
delete k (Balance xs) = Balance (IntMap.delete k xs)

instance K.Indexable Balance where
    index Zero _         = error "Key not in zero Balance"
    index (Plain _) _    = error "Key not in plain Balance"
    index (Amount c x) k = if c == k
                           then x
                           else error "Key not in zero Balance"
    index (Balance xs) k = K.index xs k

type instance Index (Balance a) = Int
type instance IxValue (Balance a) = a
instance Applicative f => Ixed f (Balance a) where
    ix _k _f Zero = pure Zero
    ix _k _f pl@(Plain _) = pure pl
    ix k f amt@(Amount c q)
        | k == c    = Amount c <$> (Lens.indexed f k q <&> id)
        | otherwise = pure amt
    ix k f bal@(Balance xs) = case IntMap.lookup k xs of
        Just v  -> Balance
            <$> (Lens.indexed f k v <&> \v' -> IntMap.insert k v' xs)
        Nothing -> pure bal

instance At (Balance a) where
    at k f m = Lens.indexed f k mv <&> \r -> case r of
        Nothing -> maybe m (const (delete k m)) mv
        Just v' -> insert k v' m
      where mv = K.lookup k m

instance (Contravariant f, Functor f) => Contains f (Balance a) where
  contains = containsLookup K.lookup
  {-# INLINE contains #-}

instance Applicative f => Each f (Balance a) (Balance b) a b where
  each _ Zero = pure Zero
  each f (Plain q) = Plain <$> Lens.indexed f noCommodity q
  each f (Amount c q) = Amount c <$> Lens.indexed f c q
  each f (Balance m) = sequenceA $ Balance $ IntMap.mapWithKey f' m
    where f' = Lens.indexed f
  {-# INLINE each #-}

instance FunctorWithIndex Int Balance where
  imap = iover itraversed
  {-# INLINE imap #-}

instance FoldableWithIndex Int Balance where
  ifoldMap = ifoldMapOf itraversed
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Int Balance where
  itraverse = itraverseOf traversed
  {-# INLINE itraverse #-}

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

instance Num a => Semigroup (Balance a) where
    Zero <> x    = x
    y    <> Zero = y

    Plain qx     <> Plain qy     = Plain $ qx + qy
    Plain qx     <> Amount cy qy = Amount cy (qx + qy)
    Amount cx qx <> Plain qy     = Amount cx (qx + qy)
    Plain qx     <> y            = Amount noCommodity qx `mappend` y
    x            <> Plain qy     = x `mappend` Amount noCommodity qy

    Amount cx qx <> Amount cy qy
        | cx == cy  = Amount cx (qx + qy)
        | otherwise = Balance (IntMap.fromList [(cx,qx),(cy,qy)])

    Amount cx qx <> Balance ys   = Balance (IntMap.singleton cx qx <> ys)
    Balance xs   <> Amount cy qy = Balance (xs <> IntMap.singleton cy qy)

    Balance xs <> Balance ys = Balance (xs <> ys)

instance Num a => Monoid (Balance a) where
    mempty = Zero
    mappend x y = x <> y

class Monoid g => Group g where
    inverse :: g -> g

instance Num a => Group (Balance a) where
    inverse x = Zero ^-^ x

balanceStore :: K.Indexable f => K.Key f -> f a -> Store (K.Key f) a
balanceStore k x = store (K.index x) k

balanceSum :: Num a => [Balance a] -> Balance a
balanceSum = Foldable.foldr (^+^) Zero
