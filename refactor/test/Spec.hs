{-# LANGUAGE OverloadedStrings #-}

import Data.FuzzySet
import Data.FuzzySet.Internal
import Data.FuzzySet.Types
import Data.FuzzySet.Util
import Helpers
import Test.Hspec
import qualified Data.HashMap.Strict as HashMap


main :: IO ()
main = 
    let 
        detectives = defaultSet `add` "Bruce Wayne" `add` "Charlie Chan" `add` "Frank Columbo" `add` "Hercule Poirot" `add` "Jane Marple" `add` "Lisbeth Salander" `add` "Nancy Drew" `add` "Nero Wolfe" `add` "Perry Mason" `add` "Philip Marlowe" `add` "Sherlock Holmes"
        detectivesDict = matchDict detectives
        Just map1 = HashMap.lookup "olm" detectivesDict
        Just map2 = HashMap.lookup "-n" detectivesDict
        Just map3 = HashMap.lookup "y " detectivesDict
        Just map4 = HashMap.lookup "wa" detectivesDict
        Just map5 = HashMap.lookup "ne" detectivesDict
        Just map6 = HashMap.lookup "ch" detectivesDict
        Just map7 = HashMap.lookup "cha" detectivesDict

        scores1 =
            [ ( 0.17677669529663687, "sherlock holmes" )
            , ( 0.10660035817780521, "nero wolfe" )
            , ( 0.10206207261596574, "bruce wayne" )
            , ( 0.10206207261596574, "jane marple" )
            , ( 0.0944911182523068, "frank columbo" )
            , ( 0.09128709291752767, "philip marlowe" )
            ]

    in 
    hspec $ do
        describe "matches" $ do
            it "Watson" $ do
                HashMap.fromList [(0,1),(1,1),(8,3)] `shouldBe` matches detectives (gramMap "Watson" 2)
                HashMap.fromList [(8,2)] `shouldBe` matches detectives (gramMap "Watson" 3)

            it "Gumshoe" $ do
                HashMap.fromList [(0,1),(2,1),(4,1),(7,1),(9,1),(10,2)] `shouldBe` matches detectives (gramMap "Gumshoe" 2)

        describe "matchDict" $ do
            it "lookup \"olm\"" $ do
                GramInfo 10 1 `shouldBeIn` map1

            it "lookup \"-n\"" $ do
                GramInfo 6 1 `shouldBeIn` map2
                GramInfo 7 1 `shouldBeIn` map2

            it "lookup \"y \"" $ do
                GramInfo 6 1 `shouldBeIn`  map3
                GramInfo 8 1 `shouldBeIn`  map3

            it "lookup \"wa\"" $ do
                GramInfo 0 1 `shouldBeIn` map4

            it "lookup \"ne\"" $ do
                GramInfo 0 1 `shouldBeIn` map5
                GramInfo 4 1 `shouldBeIn` map5
                GramInfo 7 1 `shouldBeIn` map5

            it "lookup \"ch\"" $ do
                GramInfo 1 2 `shouldBeIn` map6

            it "lookup \"cha\"" $ do
                GramInfo 1 2 `shouldBeIn` map7

        describe "norm" $ do
            it "[2, 4, 3, 3, 3, 3, 2, 3, 2, 2, 2] should equal 9" $
                9 `shouldBeCloseTo` norm [2, 4, 3, 3, 3, 3, 2, 3, 2, 2, 2]

        describe "enclosedIn" $ do
            it "\"covfefe\" 'o' should return ocovfefeo" $
                "ocovfefeo" `shouldBe` ("covfefe" `enclosedIn` 'o')
