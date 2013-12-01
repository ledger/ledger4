{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Trans.State
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Ratio
import           Data.Thyme
import           Data.Thyme.Time
import           Ledger.Commodity
import           Ledger.Commodity.History
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Sample tests" $ do
        it "returns the first element of a list" $
            head [23 ..] `shouldBe` (23 :: Int)

        it "returns the first element of an *arbitrary* list" $
            property $ \x xs -> head (x:xs) == (x :: Int)

        it "throws an exception if used with an empty list" $
            evaluate (head []) `shouldThrow` anyException

    describe "Ledger.Commodity.History" $ do
        it "works with testMap" $ do
            let cm = testMap moment
            findConversion 1 3 moment cm
                `shouldBe` Just (oneHourAgo, 400 % 249)
            findConversion 1 3 (addUTCTime (-7000) moment) cm
                `shouldBe` Just (oneDayAgo, 50 % 33)

        it "works with testMap'" $ do
            let cm' = testMap' moment
            findConversion 1 3 moment cm'
                `shouldBe` Just (oneHourAgo, 8 % 5)
            findConversion 1 3 (addUTCTime (-7000) moment) cm'
                `shouldBe` Just (oneDayAgo, 3 % 2)
  where
    moment = UTCTime (ModifiedJulianDay 50000) (secondsToDiffTime 0)
        ^. from utcTime
    oneHourAgo = addUTCTime (-3600) moment
    oneDayAgo  = addUTCTime (-(24 * 3600)) moment

testMap :: UTCTime -> CommodityMap
testMap now =
    let oneHourAgo = addUTCTime (-3600) now
        oneDayAgo  = addUTCTime (-(24 * 3600)) now

        usd = (defaultPrimary "USD")
            { commHistory =
                   IntMap.fromList [ (2, Map.fromList [(oneHourAgo, 0.75)])
                                   , (3, Map.fromList [(oneDayAgo, 0.66)])
                                   ]
            }
        cad = (defaultPrimary "CAD")
            { commHistory =
                IntMap.fromList [ (1, Map.fromList [(oneHourAgo, 1.33)])
                                , (3, Map.fromList [(oneHourAgo, 0.83)])
                                ]
            }
        eur = (defaultPrimary "EUR")
            { commHistory =
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
