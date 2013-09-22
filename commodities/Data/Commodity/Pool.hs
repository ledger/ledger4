{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Commodity.Pool
       ( Commodity
       , CommodityInfo(..)
       , defaultCommodityInfo
       , CommodityPool(..)
       , findConversion
       , addConversion
       ) where

import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.IntMap (IntMap, Key)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.PSQueue (PSQ, Binding(..), minView)
import qualified Data.PSQueue as PSQ
import           Data.Ratio
import           Data.Text (Text)
import           Data.Time

-- | Commodities are simply indices into a commodity pool, which relates such
--   commodities to the information known about them.
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

-- | A pool of commodities, relating commodity indices to information about
--   those commodities.
data CommodityPool = CommodityPool
    { poolCommodities :: IntMap CommodityInfo
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
               -> CommodityPool -- ^ Pool of commodities to search
               -> Maybe (UTCTime, Rational)
findConversion from to time pool = go <$> intAStar g d h (== to) from
  where
    g s = IntMap.keysSet $ IntMap.filter (isJust . Map.lookupLT time)
                         $ conversions s

    d s t = let (time', _) = conv s t in diffUTCTime time time'

    h _goal = 0                 -- is a heuristic even possible here?

    go = (\(x, y, _) -> (x, y)) . foldl' f (time, 1, from)
      where
        f (w, r, from') c = let (time', r') = conv from' c
                            in (if w < time' then w else time', r / r', c)

    conv s t = let m = conversions s IntMap.! t
               in fromMaybe (error "Not possible") $ Map.lookupLE time m

    conversions = commConversions . (poolCommodities pool IntMap.!)

-- | Add a price conversion in the form of a ratio between two commodities at
--   a specific point in time.
addConversion :: Commodity -> Commodity -> UTCTime -> Rational
              -> State CommodityPool ()
addConversion from to time ratio =
    modify $ \(poolCommodities -> pool) ->
        CommodityPool $
            update (1/ratio) to from $ update ratio from to pool
  where
    update r s t = IntMap.adjust (flip (addconv r) t) s

    addconv r s t =
        let c  = commConversions s
            mm = IntMap.lookup t c
            rm = case mm of
                Nothing -> Map.singleton time r
                Just m  -> Map.insert time r m
        in s { commConversions = IntMap.insert t rm c }

testPool :: UTCTime -> CommodityPool
testPool now =
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
    in CommodityPool $ IntMap.fromList
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

testPool' :: UTCTime -> CommodityPool
testPool' now =
    let usd = defaultPrimary "USD"
        cad = defaultPrimary "CAD"
        eur = defaultPrimary "EUR"

        pool = CommodityPool $ IntMap.fromList
            [ (1, usd)
            , (2, cad)
            , (3, eur)
            ]
    in flip execState pool $ do
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

runTestPool :: IO ()
runTestPool = do
    now <- getCurrentTime

    let pool = testPool now
    print pool
    print $ findConversion 1 3 now pool
    print $ findConversion 1 3 (addUTCTime (-7000) now) pool

    let pool' = testPool now
    print pool'
    print $ findConversion 1 3 now pool'
    print $ findConversion 1 3 (addUTCTime (-7000) now) pool'
