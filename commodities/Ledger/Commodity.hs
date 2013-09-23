{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Ledger.Commodity
       ( Commodity
       , CommodityInfo(..)
       , defaultCommodityInfo
       , CommodityMap(..)
       , findConversion
       , addConversion
       , intAStar
       , intAStarM
       ) where

import           Control.Applicative
import           Control.Monad hiding (forM)
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import           Data.IntMap (IntMap, Key)
import qualified Data.IntMap as IntMap
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.PSQueue (PSQ, Binding(..), minView)
import qualified Data.PSQueue as PSQ
import           Data.Ratio
import           Data.Text (Text)
import           Data.Time
import           Data.Traversable
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
    { visited  :: !(IntMap c)
    , waiting  :: !(PSQ Key c)
    , score    :: !(IntMap c)
    , memoHeur :: !(IntMap c)
    , cameFrom :: !(IntMap Key)
    , end      :: !(Maybe Key)
    } deriving Show

intAStarInit :: (Num c, Ord c) => Key -> IntAStar c
intAStarInit start = IntAStar
    { visited  = IntMap.empty
    , waiting  = PSQ.singleton start 0
    , score    = IntMap.singleton start 0
    , memoHeur = IntMap.empty
    , cameFrom = IntMap.empty
    , end      = Nothing
    }

runIntAStarM :: (Monad m, Ord c, Num c)
             => (Key -> m (IntMap c))   -- adjacencies in graph
             -> (Key -> m c)      -- heuristic distance to goal
             -> (Key -> m Bool)   -- goal
             -> Key               -- starting vertex
             -> m (IntAStar c)   -- final state
runIntAStarM graph heur goal start = aStar' (intAStarInit start)
  where
    aStar' s = case minView (waiting s) of
        Nothing -> return s
        Just (x :-> d, w') -> do
            g <- goal x
            if g
                then return (s { end = Just x })
                else do
                    ns <- graph x
                    u <- foldM (expand x)
                        (s { waiting = w'
                           , visited = IntMap.insert x d (visited s)
                           })
                         (IntMap.toList (ns IntMap.\\ visited s))
                    aStar' u

    expand x s (y,d) = do
        let v = score s IntMap.! x + d
        case PSQ.lookup y (waiting s) of
            Nothing -> do
                h <- heur y
                return $ link x y v
                    (s { memoHeur = IntMap.insert y h (memoHeur s) })
            Just _  -> return $ if v < score s IntMap.! y
                                then link x y v s
                                else s
    link x y v s
       = s { cameFrom = IntMap.insert y x (cameFrom s),
             score    = IntMap.insert y v (score s),
             waiting  = PSQ.insert y (v + memoHeur s IntMap.! y) (waiting s) }

-- | This function computes an optimal (minimal distance) path through a graph
-- in a best-first fashion, starting from a given starting point.
intAStarM
    :: (Monad m, Ord c, Num c)
    => (Key -> m (IntMap c))   -- ^ The graph we are searching through, given as
                               -- a function from vertices to their neighbours.
    -> (Key -> m c)            -- ^ Heuristic distance to the (nearest) goal.
                               -- This should never overestimate the distance,
                               -- or else the path found may not be minimal.
    -> (Key -> m Bool)         -- ^ The goal, specified as a boolean predicate
                               -- on vertices.
    -> m Key                   -- ^ The vertex to start searching from.
    -> m (Maybe [Key])         -- ^ An optimal path, if any path exists. This
                               -- excludes the starting vertex.
intAStarM graph heur goal start = do
    sv <- start
    s  <- runIntAStarM graph heur goal sv
    forM (end s) $ \e ->
        return . reverse
               . takeWhile (not . (== sv))
               . iterate (cameFrom s IntMap.!)
               $ e

-- | This function computes an optimal (minimal distance) path through a graph
--   in a best-first fashion, starting from a given starting point.
intAStar :: (Ord c, Num c)
         => (Key -> IntMap c) -- ^ The graph we are searching through, given as
                              -- a function from vertices to their neighbours.
         -> (Key -> c)        -- ^ Heuristic distance to the (nearest) goal.
                              -- This should never overestimate the distance, or
                              -- else the path found may not be minimal.
         -> (Key -> Bool)     -- ^ The goal, specified as a boolean predicate on
                              -- vertices.
         -> Key               -- ^ The vertex to start searching from.
         -> Maybe [Key]       -- ^ An optimal path, if any path exists. This
                              -- excludes the starting vertex.
intAStar graph heur goal start =
    runIdentity $ intAStarM
        (return . graph)
        (return . heur)
        (return . goal)
        (return start)

-- | Lookup a price conversion from the source commodity to the target, using
--   data from the given time or earlier.  Result is Nothing if no conversion
--   can be found, or else the best conversion ratio plus the time of the
--   oldest link.
findConversion :: Commodity     -- ^ Source commodity
               -> Commodity     -- ^ Target commodity
               -> UTCTime       -- ^ Look for conversions on or before this
               -> CommodityMap  -- ^ Set of commodities to search
               -> Maybe (UTCTime, Rational)
findConversion from to time cm =
    let (keyPath, valuesMap) =
            flip runState IntMap.empty $
                intAStarM g h (return . (== to)) (return from)
    in go valuesMap <$> keyPath
  where
    g c = do
        vm <- get
        let (!m, !sm) = IntMap.foldlWithKey'
                (\(!m', !sm') k cs ->
                  case Map.lookupLE time cs of
                      Nothing    -> (m', sm')
                      Just (t,r) ->
                          (IntMap.insert k (diffUTCTime time t) m',
                           IntMap.insert k (t, r) sm'))
                (IntMap.empty, IntMap.empty)
                (commConversions $ commodities cm IntMap.! c)
        put $! IntMap.insert c sm vm
        return m

    h _goal = return 0

    go vm ks = (\(!x, !y, _) -> (x, y)) $ foldl' f (time, 1, from) ks
      where
        f (!w, !r, !s) t = let (w', r') = vm IntMap.! s IntMap.! t
                           in (min w w', r / r', t)

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
