{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Ledger.Commodity.History
       ( findConversion
       , addConversion
       , intAStar
       , intAStarM
       ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad hiding (forM)
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import           Data.IntMap (IntMap, Key)
import qualified Data.IntMap as IntMap
import           Data.List (foldl')
import qualified Data.Map as Map
import           Data.PSQueue (PSQ, Binding(..), minView)
import qualified Data.PSQueue as PSQ
import           Data.Ratio
import           Data.Thyme.Time
import           Data.Traversable
import           Ledger.Commodity
import           Prelude hiding (lookup)

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
findConversion f t time cm =
    let (keyPath, valuesMap) =
            flip runState IntMap.empty $
                intAStarM g h (return . (== t)) (return f)
    in go valuesMap <$> keyPath
  where
    g c = do
        vm <- get
        let (!m, !sm) = IntMap.foldlWithKey'
                (\(!m', !sm') k cs ->
                  case Map.lookupLE time cs of
                      Nothing    -> (m', sm')
                      Just (u,r) ->
                          (IntMap.insert k (diffUTCTime time u) m',
                           IntMap.insert k (u, r) sm'))
                (IntMap.empty, IntMap.empty)
                (cm ^. commodities.ix c.commHistory)
        put $! IntMap.insert c sm vm
        return m

    h _goal = return 0

    go vm ks = (\(!x, !y, _) -> (x, y)) $ foldl' h (time, 1, f) ks
      where
        h (!w, !r, !s) u = let (w', r') = vm IntMap.! s IntMap.! u
                           in (min w w', r / r', u)

-- | Add a price conversion in the form of a ratio between two commodities at
--   a specific point in time.
addConversion :: Commodity -> Commodity -> UTCTime -> Rational
              -> State CommodityMap ()
addConversion f t time ratio = do
    commodities.at t %= fmap (addconv (1/ratio) ?? f)
    commodities.at f %= fmap (addconv ratio ?? t)
  where
    addconv r s t =
        let c  = s^.commHistory
            rm = case IntMap.lookup t c of
                Nothing -> Map.singleton time r
                Just m  -> Map.insert time r m
        in s & commHistory .~ IntMap.insert t rm c
