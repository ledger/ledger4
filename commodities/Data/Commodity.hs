{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Commodity
       ( Commodity
       , CommodityInfo(..)
       , defaultCommodityInfo
       , CommodityMap(..)
       , findConversion
       , addConversion
       ) where

import           Control.Applicative
--import           Control.Comonad.Trans.Store
import           Control.Lens
import           Control.Monad.Trans.State
import           Data.Data
--import           Data.Foldable as Foldable hiding (toList, foldl')
import           Data.Functor.Bind as Bind
import           Data.IntMap (IntMap, Key)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
--import qualified Data.Key as K
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isJust)
--import           Data.Monoid
import           Data.PSQueue (PSQ, Binding(..), minView)
import qualified Data.PSQueue as PSQ
import           Data.Ratio
import           Data.Text (Text)
import           Data.Time
--import           Data.Traversable as Traversable
import           Linear.Vector
import           Prelude hiding (lookup)

-- | Commodities are simply indices into a commodity info map, which relates
--   such commodities to the information known about them.
type Commodity = Key

-- | All of the information known about a commodity.
data CommodityInfo = CommodityInfo
    { commSymbol       :: !Text
    , commPrecision    :: !Int
    , commSuffixed     :: !Bool
    , commSeparated    :: !Bool
    , commThousands    :: !Bool
    , commDecimalComma :: !Bool
    , commNoMarket     :: !Bool
    , commBuiltin      :: !Bool
    , commKnown        :: !Bool
    , commPrimary      :: !Bool
    , commConversions  :: !(IntMap (Map UTCTime Rational))
    } deriving Show

-- | Return a 'CommodityInfo' with defaults selected for all fields.  It is
--   intended that at least one field of the result will be modified
--   immediately.
defaultCommodityInfo :: CommodityInfo
defaultCommodityInfo = CommodityInfo
    { commSymbol       = ""
    , commPrecision    = 0
    , commSuffixed     = False
    , commSeparated    = False
    , commThousands    = True
    , commDecimalComma = False
    , commNoMarket     = False
    , commBuiltin      = False
    , commKnown        = False
    , commPrimary      = False
    , commConversions  = IntMap.empty
    }

-- | A commodities map, relating commodity indices to information about
--   those commodities.
data CommodityMap = CommodityMap
    { commodities :: IntMap CommodityInfo
    } deriving Show

-- | The following A* algorithm was written by Cale Gibbard, and modified here
--   to apply to IntMap's instead of general Map's.
data IntAStar c = IntAStar
    { visited  :: !IntSet
    , waiting  :: !(PSQ Key c)
    , score    :: !(IntMap c)
    , memoHeur :: !(IntMap c)
    , cameFrom :: !(IntMap Key)
    , end      :: !(Maybe Key)
    } deriving Show

intAStarInit :: (Num c, Ord c) => Key -> IntAStar c
intAStarInit start = IntAStar
    { visited  = IntSet.empty
    , waiting  = PSQ.singleton start 0
    , score    = IntMap.singleton start 0
    , memoHeur = IntMap.empty
    , cameFrom = IntMap.empty
    , end      = Nothing
    }

runIntAStar :: (Ord c, Num c)
            => (Key -> IntSet)   -- adjacencies in graph
            -> (Key -> Key -> c) -- distance function
            -> (Key -> c)        -- heuristic distance to goal
            -> (Key -> Bool)     -- goal
            -> Key               -- starting vertex
            -> IntAStar c        -- final state
runIntAStar graph dist heur goal start = aStar' (intAStarInit start)
  where
    aStar' s = case minView (waiting s) of
        Nothing -> s
        Just (x :-> _, w') ->
            if goal x
            then s { end = Just x }
            else aStar' $ foldl' (expand x)
                (s { waiting = w'
                   , visited = IntSet.insert x (visited s)
                   })
                (IntSet.toList (graph x IntSet.\\ visited s))

    expand x s y =
        let v = score s IntMap.! x + dist x y
        in case PSQ.lookup y (waiting s) of
            Nothing -> link x y v
                (s { memoHeur = IntMap.insert y (heur y) (memoHeur s) })
            Just _  -> if v < score s IntMap.! y
                       then link x y v s
                       else s

    link x y v s = s
        { cameFrom = IntMap.insert y x (cameFrom s)
        , score    = IntMap.insert y v (score s)
        , waiting  = PSQ.insert y (v + memoHeur s IntMap.! y) (waiting s)
        }

-- | This function computes an optimal (minimal distance) path through a graph
--   in a best-first fashion, starting from a given starting point.
intAStar :: (Ord c, Num c)
         => (Key -> IntSet)   -- ^ The graph we are searching through, given as
                              -- a function from vertices to their neighbours.
         -> (Key -> Key -> c) -- ^ Distance function between neighbouring
                              -- vertices of the graph. This will never be
                              -- applied to vertices that are not neighbours, so
                              -- may be undefined on pairs that are not
                              -- neighbours in the graph.
         -> (Key -> c)        -- ^ Heuristic distance to the (nearest) goal.
                              -- This should never overestimate the distance, or
                              -- else the path found may not be minimal.
         -> (Key -> Bool)     -- ^ The goal, specified as a boolean predicate on
                              -- vertices.
         -> Key               -- ^ The vertex to start searching from.
         -> Maybe [Key]       -- ^ An optimal path, if any path exists. This
                              -- excludes the starting vertex.
intAStar graph dist heur goal start =
    let s = runIntAStar graph dist heur goal start
    in case end s of
        Nothing -> Nothing
        Just e  -> Just (reverse . takeWhile (not . (== start))
                                 . iterate (cameFrom s IntMap.!) $ e)

-- | Lookup a price conversion from the source commodity to the target, using
--   data from the given time or earlier.  Result is Nothing if no conversion
--   can be found, or else the best conversion ratio plus the time of the
--   oldest link.
findConversion :: Commodity     -- ^ Source commodity
               -> Commodity     -- ^ Target commodity
               -> UTCTime       -- ^ Look for conversions on or before this
               -> CommodityMap  -- ^ Set of commodities to search
               -> Maybe (UTCTime, Rational)
findConversion from to time cm = go <$> intAStar g d h (== to) from
  where
    g s = IntMap.keysSet $ IntMap.filter (isJust . Map.lookupLT time)
                         $ conversions s

    d s t = let (time', _) = conv s t in diffUTCTime time time'

    h goal = IntMap.foldl'
        (Map.foldlWithKey' $ \diff t _ ->
          if t > time
          then diff
          else min diff (diffUTCTime time t))
        (diffUTCTime time minTime)
        (conversions goal)

    minTime = UTCTime (ModifiedJulianDay 0) 0

    go = (\(x, y, _) -> (x, y)) . foldl' f (time, 1, from)
      where
        f (w, r, from') c = let (time', r') = conv from' c
                            in (min w time', r / r', c)

    conv s t = let m = conversions s IntMap.! t
               in fromMaybe (error "Not possible") $ Map.lookupLE time m

    conversions = commConversions . (commodities cm IntMap.!)

-- | Add a price conversion in the form of a ratio between two commodities at
--   a specific point in time.
addConversion :: Commodity -> Commodity -> UTCTime -> Rational
              -> State CommodityMap ()
addConversion from to time ratio =
    modify $ \(commodities -> cm) ->
        CommodityMap $ update (1/ratio) to from $ update ratio from to cm
  where
    update r s t = IntMap.adjust (flip (addconv r) t) s

    addconv r s t =
        let c  = commConversions s
            mm = IntMap.lookup t c
            rm = case mm of
                Nothing -> Map.singleton time r
                Just m  -> Map.insert time r m
        in s { commConversions = IntMap.insert t rm c }

testMap :: UTCTime -> CommodityMap
testMap now =
    let oneHourAgo = addUTCTime (-3600) now
        oneDayAgo  = addUTCTime (-(24 * 3600)) now

        usd = (defaultPrimary "USD")
            { commConversions =
                   IntMap.fromList [ (2, Map.fromList [(oneHourAgo, 0.75)])
                                   , (3, Map.fromList [(oneDayAgo, 0.66)])
                                   ]
            }
        cad = (defaultPrimary "CAD")
            { commConversions =
                IntMap.fromList [ (1, Map.fromList [(oneHourAgo, 1.33)])
                                , (3, Map.fromList [(oneHourAgo, 0.83)])
                                ]
            }
        eur = (defaultPrimary "EUR")
            { commConversions =
                IntMap.fromList [ (1, Map.fromList [(oneDayAgo, 1.5)])
                                , (2, Map.fromList [(oneHourAgo, 1.2)])
                                ]
            }
    in CommodityMap $ IntMap.fromList
        [ (1, usd)
        , (2, cad)
        , (3, eur)
        ]
  where
    defaultPrimary sym = defaultCommodityInfo
        { commSymbol    = sym
        , commPrecision = 2
        , commNoMarket  = True
        , commKnown     = True
        , commPrimary   = True
        }

testMap' :: UTCTime -> CommodityMap
testMap' now =
    let usd = defaultPrimary "USD"
        cad = defaultPrimary "CAD"
        eur = defaultPrimary "EUR"

        cm = CommodityMap $ IntMap.fromList
            [ (1, usd)
            , (2, cad)
            , (3, eur)
            ]
    in flip execState cm $ do
        let oneHourAgo = addUTCTime (-3600) now
            oneDayAgo  = addUTCTime (-(24 * 3600)) now
        addConversion 1 2 oneHourAgo (3 % 4)
        addConversion 1 3 oneDayAgo  (2 % 3)
        addConversion 2 3 oneHourAgo (5 % 6)
  where
    defaultPrimary sym = defaultCommodityInfo
        { commSymbol    = sym
        , commPrecision = 2
        , commNoMarket  = True
        , commKnown     = True
        , commPrimary   = True
        }

runTestMap :: IO ()
runTestMap = do
    now <- getCurrentTime

    let cm = testMap now
    print cm
    print $ findConversion 1 3 now cm
    print $ findConversion 1 3 (addUTCTime (-7000) now) cm

    let cm' = testMap now
    print cm'
    print $ findConversion 1 3 now cm'
    print $ findConversion 1 3 (addUTCTime (-7000) now) cm'

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

-- instance Bind Balance where
--     Zero >>- _       = Zero
--     Plain q >>- f    = f q
--     Amount _ q >>- f = f q
--     Balance xs >>- f = IntMap.foldlWithKey' (\acc k x -> f x ^+^ acc) Zero xs

-- instance Monad Balance where
--     return = pure
--     (>>=) = (>>-)

-- type instance K.Key Balance = IntMap.Key

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
