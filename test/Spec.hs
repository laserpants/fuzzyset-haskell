{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Control.Exception             ( evaluate )
import Control.Lens
import Data.AEq
import Data.FuzzySet
import Data.FuzzySet.Lens
import Data.FuzzySet.Types
import Data.FuzzySet.Util
import Data.HashMap.Strict           ( HashMap, fromList, empty )
import Data.Maybe
import Data.Monoid
import Data.Monoid.Unicode
import Data.Text                     ( Text, pack, unpack )
import Prelude.Unicode
import Test.Hspec

import qualified Data.Text           as Text
import qualified Data.HashMap.Strict as Map

shouldBeTrue ∷ Bool → Expectation
shouldBeTrue = shouldBe True

shouldBeCloseTo ∷ Double → Double → Expectation
shouldBeCloseTo q r = shouldBeTrue (q ~== r)

lookup0 ∷ Text → HashMap Text Int → Int
lookup0 = Map.lookupDefault 0

checkGramsCount ∷ Text → Size → SpecWith ()
checkGramsCount input n = it message $ do
    length list `shouldBe` expectedLen
    shouldBeTrue $ all (≡n) (Text.length <$> list)
  where
    list = grams input n
    message = "should return a list of length "
            ⊕ show expectedLen
            ⊕ ", given the input \""
            ⊕ unpack input ⊕ "\" and n = " ⊕ show n
    expectedLen = s - n + 3
    s = Text.length input

checkMapKey ∷ HashMap Text Int → Text → Int → SpecWith ()
checkMapKey grams key value =
    it message (lookup0 key grams `shouldBe` value)
  where
    message = "should map they key \""
            ⊕ unpack key ⊕ "\" to "
            ⊕ show value

vectorMagnitudeOfItem ∷ FuzzySet → Int → Int → Maybe Double
vectorMagnitudeOfItem set n p = vectorMagnitude <$> set^._items.at n._Just^?ix p

checkMagnitude ∷ FuzzySet → Int → Int → Double → SpecWith ()
checkMagnitude set n p r =
    it message $ fromMaybe 0 (vectorMagnitudeOfItem set n p) `shouldBeCloseTo` r
  where
    message = "should return vectorMagnitude = " ⊕ show r
            ⊕ " for the " ⊕ show n ⊕ "-grams entry (index " ⊕ show p ⊕ ")"

checkMatchDictEntry ∷ FuzzySet → Text → [GramInfo] → SpecWith ()
checkMatchDictEntry set gram entry =
    it message (set^._matchDict.ix gram `shouldBe` entry)
  where
    message = "should return a match dict entry "
            ⊕ show entry ⊕ " for " ⊕ show gram

checkExactSet ∷ FuzzySet → [(Text, Text)] → SpecWith ()
checkExactSet set xs =
    it ("should have exactSet = " ⊕ show xs) $
      exactSet set `shouldBe` fromList xs

checkGrams ∷ Text → Size → [Text] → SpecWith ()
checkGrams txt size r =
    describe msg $ it ("should return " ⊕ show r) (grams txt size `shouldBe` r)
  where
    msg = "grams " ⊕ show txt ⊕ " " ⊕ show size

checkGramMap ∷ Text → Size → [(Text, Int)] → SpecWith ()
checkGramMap txt size r =
    describe msg $ it ("should return " ⊕ show r)
                      (gramMap txt size `shouldBe` fromList r)
  where
    msg = "gramMap " ⊕ show txt ⊕ " " ⊕ show size

checkGramMapKeys ∷ Text → Size → [(Text, Int)] → SpecWith ()
checkGramMapKeys txt size keys =
    describe msg $ mapM_ (uncurry $ checkMapKey grams) keys
  where
    msg = "gramMap " ⊕ show txt ⊕ " " ⊕ show size
    grams = gramMap txt size

checkMatches ∷ FuzzySet → Text → Size → [(Int, Int)] → SpecWith ()
checkMatches set txt size r =
    describe msg $ it ("should return " ⊕ show r) $
      matches (set^._matchDict) (gramMap txt size) `shouldBe` fromList r
  where
    msg = "matches "  ⊕ show (set^._exactSet) ⊕ " "
        ⊕ "(gramMap " ⊕ show txt ⊕ " " ⊕ show size ⊕ ")"

main ∷ IO ()
main = hspec $ do

    describe "grams" $ do
      mapM_ (checkGramsCount "charade") [2..6]
      it "should throw an error if n < 2" $
        evaluate (grams "anything" 1) `shouldThrow` anyException

    checkGrams "charade" 2 ["-c", "ch", "ha", "ar", "ra", "ad", "de", "e-"]
    checkGrams "charade" 2 ["-c", "ch", "ha", "ar", "ra", "ad", "de", "e-"]
    checkGrams "charade" 3 ["-ch", "cha", "har", "ara", "rad", "ade", "de-"]
    checkGrams "aFl1pP!.,nG FL0^ppy+" 2
        [ "-a", "af", "fl", "l1", "1p", "pp", "p,", ",n", "ng", "g ", " f"
        , "fl", "l0", "0p", "pp", "py", "y-" ]

    checkGramMap "xxx" 2 [("-x", 1),("xx", 2),("x-", 1)]
    checkGramMap "xxx" 3 [("-xx", 1), ("xx-", 1), ("xxx", 1)]
    checkGramMap "xxxxxxx" 4 [("-xxx", 1), ("xxxx", 4), ("xxx-", 1)]
    checkGramMap "bananasananas" 2
        [ ("-b", 1), ("ba", 1), ("an", 4), ("na", 4), ("as", 2)
        , ("sa", 1), ("s-", 1) ]
    checkGramMap "bananasananas" 3
        [ ("-ba", 1), ("ban", 1), ("ana", 4), ("nan", 2), ("nas", 2)
        , ("asa", 1), ("san", 1), ("as-", 1) ]

    checkGramMapKeys "trentsauntsrestaurant" 2
      [ ("nt", 3)
      , ("au", 2)
      , ("ts", 2)
      , ("re", 2)
      , ("st", 1)
      , ("en", 1) ]
    checkGramMapKeys "trentsauntsrestaurant" 3
      [ ("res", 1)
      , ("nts", 2) ]
    checkGramMapKeys "trentsantwantstorentpants" 3
      [ ("pan", 1)
      , ("twa", 1)
      , ("ant", 3)
      , ("ren", 2)
      , ("ent", 2)
      , ("nts", 3) ]
    checkGramMapKeys "trentsantwantstorentpantstostartrestaurant" 3
      [ ("ant", 4)
      , ("nts", 3)
      , ("sto", 2)
      , ("sta", 2)
      , ("ren", 2)
      , ("tre", 2) ]
    checkGramMapKeys "trentsantwantstorentpantstostartrestaurant" 2
      [ ("an", 4)
      , ("st", 4)
      , ("re", 3)
      , ("ts", 3)
      , ("en", 2)
      , ("to", 2)
      , ("tr", 2)
      , ("or", 1)
      , ("au", 1)
      , ("ur", 1) ]
    checkGramMapKeys "antsintrentspantswanttrentsauntsrestaurant" 3
      [ ("nts", 5)
      , ("ant", 4)
      , ("ent", 2) ]
    checkGramMapKeys "asmartantintrentspantswantstorenttrentsauntsrestaurant" 3
      [ ("nts", 5)
      , ("ant", 4)
      , ("ent", 3) ]
    checkGramMapKeys "buffalo buffalo buffalo buffalo buffalo buffalo" 7
      [ ("buffalo", 6) ]

    describe "addToSet defaultSet \"aFl1pP!.,nG FL0^ppy+\"" $
      let (set, changed) = addToSet defaultSet "aFl1pP!.,nG FL0^ppy+"
       in do
        it "should return changed status True" $ shouldBeTrue changed
        checkExactSet set [("afl1pp!.,ng fl0^ppy+", "aFl1pP!.,nG FL0^ppy+")]
        checkMagnitude set 2 0 4.58257569495584
        checkMagnitude set 3 0 4.0
        checkMatchDictEntry set "-a" [GramInfo 0 1]
        checkMatchDictEntry set "ng" [GramInfo 0 1]
        checkMatchDictEntry set "fl" [GramInfo 0 2]
        checkMatchDictEntry set "pp" [GramInfo 0 2]
        checkMatchDictEntry set "g " [GramInfo 0 1]
        checkMatchDictEntry set "xx" []

    describe "addToSet defaultSet \"Trent\"" $
      let (set, changed) = addToSet defaultSet "Trent"
       in do
        it "should return changed status True" $ shouldBeTrue changed
        checkExactSet set [("trent", "Trent")]
        checkMagnitude set 2 0 2.449489742783178
        checkMagnitude set 3 0 2.23606797749979
        checkMatchDictEntry set "en" [GramInfo 0 1]

    describe "defaultSet `add` \"Trent\" `add` \"tent\"" $
      let set = defaultSet `add` "Trent" `add` "tent"
       in do
        checkExactSet set [("trent", "Trent"), ("tent", "tent")]
        checkMagnitude set 2 0 2.449489742783178
        checkMagnitude set 2 1 2.23606797749979
        checkMagnitude set 3 0 2.23606797749979
        checkMagnitude set 3 1 2.0
        checkMatchDictEntry set "en"  [GramInfo 0 1, GramInfo 1 1]
        checkMatchDictEntry set "ent" [GramInfo 0 1, GramInfo 1 1]
        checkMatchDictEntry set "ten" [GramInfo 1 1]
        checkMatchDictEntry set "-t"  [GramInfo 0 1, GramInfo 1 1]

    describe "defaultSet `add` \"Trent\" `add` \"tent\" `add` \"restaurant\"" $
      let set = defaultSet `add` "Trent" `add` "tent" `add` "restaurant"
       in do
        checkExactSet set
          [ ("trent"      , "Trent")
          , ("tent"       , "tent")
          , ("restaurant" , "restaurant") ]
        checkMagnitude set 2 0 2.449489742783178
        checkMagnitude set 2 1 2.23606797749979
        checkMagnitude set 2 2 3.3166247903554
        checkMagnitude set 3 0 2.23606797749979
        checkMagnitude set 3 1 2.0
        checkMagnitude set 3 2 3.1622776601683795
        checkMatchDictEntry set "tau" [GramInfo 2 1]
        checkMatchDictEntry set "en"  [GramInfo 0 1, GramInfo 1 1]
        checkMatchDictEntry set "ten" [GramInfo 1 1]
        checkMatchDictEntry set "ran" [GramInfo 2 1]
        checkMatchDictEntry set "an"  [GramInfo 2 1]
        checkMatchDictEntry set "ant" [GramInfo 2 1]
        checkMatchDictEntry set "nt-" [GramInfo 0 1, GramInfo 1 1, GramInfo 2 1]
        checkMatchDictEntry set "st"  [GramInfo 2 1]
        checkMatchDictEntry set "es"  [GramInfo 2 1]
        checkMatchDictEntry set "est" [GramInfo 2 1]
        checkMatchDictEntry set "re"  [GramInfo 0 1, GramInfo 2 1]
        checkMatchDictEntry set "-tr" [GramInfo 0 1]
        checkMatchDictEntry set "res" [GramInfo 2 1]
        checkMatchDictEntry set "tr"  [GramInfo 0 1]
        checkMatchDictEntry set "-t"  [GramInfo 0 1, GramInfo 1 1]
        checkMatchDictEntry set "aur" [GramInfo 2 1]
        checkMatchDictEntry set "ent" [GramInfo 0 1, GramInfo 1 1]
        checkMatchDictEntry set "ra"  [GramInfo 2 1]
        checkMatchDictEntry set "-r"  [GramInfo 2 1]
        checkMatchDictEntry set "ren" [GramInfo 0 1]
        checkMatchDictEntry set "te"  [GramInfo 1 1]
        checkMatchDictEntry set "nt"  [GramInfo 0 1, GramInfo 1 1, GramInfo 2 1]

    describe "defaultSet `add` \"Trent\" `add` \"tent\" `add` \"restaurant\" `add` \"xRftAntnt,!tnRant\"" $
      let set = defaultSet `add` "Trent" `add` "tent" `add` "restaurant" `add` "xRftAntnt,!tnRant"
       in do
        checkExactSet set
          [ ("trent"             , "Trent")
          , ("tent"              , "tent")
          , ("restaurant"        , "restaurant")
          , ("xrftantnt,!tnrant" , "xRftAntnt,!tnRant") ]
        checkMagnitude set 2 0 2.449489742783178
        checkMagnitude set 2 1 2.23606797749979
        checkMagnitude set 2 2 3.3166247903554
        checkMagnitude set 2 3 5.196152422706632
        checkMagnitude set 3 0 2.23606797749979
        checkMagnitude set 3 1 2.0
        checkMagnitude set 3 2 3.1622776601683795
        checkMagnitude set 3 3 4.242640687119285
        checkMatchDictEntry set "tau" [GramInfo 2 1]
        checkMatchDictEntry set "en"  [GramInfo 0 1, GramInfo 1 1]
        checkMatchDictEntry set "ten" [GramInfo 1 1]
        checkMatchDictEntry set "ran" [GramInfo 2 1, GramInfo 3 1]
        checkMatchDictEntry set "ntn" [GramInfo 3 1]
        checkMatchDictEntry set "-xr" [GramInfo 3 1]
        checkMatchDictEntry set "an"  [GramInfo 2 1, GramInfo 3 2]
        checkMatchDictEntry set "ant" [GramInfo 2 1, GramInfo 3 2]
        checkMatchDictEntry set "t,t" [GramInfo 3 1]
        checkMatchDictEntry set "nt-" [GramInfo 0 1, GramInfo 1 1, GramInfo 2 1, GramInfo 3 1]
        checkMatchDictEntry set "nt," [GramInfo 3 1]
        checkMatchDictEntry set "xr"  [GramInfo 3 1]
        checkMatchDictEntry set "tan" [GramInfo 3 1]
        checkMatchDictEntry set "st"  [GramInfo 2 1]
        checkMatchDictEntry set "es"  [GramInfo 2 1]
        checkMatchDictEntry set "fta" [GramInfo 3 1]
        checkMatchDictEntry set "est" [GramInfo 2 1]
        checkMatchDictEntry set "re"  [GramInfo 0 1, GramInfo 2 1]
        checkMatchDictEntry set "-tr" [GramInfo 0 1]
        checkMatchDictEntry set "res" [GramInfo 2 1]
        checkMatchDictEntry set "xrf" [GramInfo 3 1]
        checkMatchDictEntry set "tr"  [GramInfo 0 1]
        checkMatchDictEntry set ",t"  [GramInfo 3 1]
        checkMatchDictEntry set "tn"  [GramInfo 3 2]
        checkMatchDictEntry set "-t"  [GramInfo 0 1, GramInfo 1 1]
        checkMatchDictEntry set "rf"  [GramInfo 3 1]
        checkMatchDictEntry set "aur" [GramInfo 2 1]
        checkMatchDictEntry set "ent" [GramInfo 0 1, GramInfo 1 1]
        checkMatchDictEntry set "ra"  [GramInfo 2 1, GramInfo 3 1]
        checkMatchDictEntry set "-r"  [GramInfo 2 1]
        checkMatchDictEntry set "-r"  [GramInfo 2 1]
        checkMatchDictEntry set "ren" [GramInfo 0 1]
        checkMatchDictEntry set "nr"  [GramInfo 3 1]
        checkMatchDictEntry set "te"  [GramInfo 1 1]
        checkMatchDictEntry set "nt"  [GramInfo 0 1, GramInfo 1 1, GramInfo 2 1, GramInfo 3 3]
        checkMatchDictEntry set "-x"  [GramInfo 3 1]
        checkMatchDictEntry set "ta"  [GramInfo 2 1, GramInfo 3 1]
        checkMatchDictEntry set "ft"  [GramInfo 3 1]
        checkMatchDictEntry set "nra" [GramInfo 3 1]
        checkMatchDictEntry set ",tn" [GramInfo 3 1]
        checkMatchDictEntry set "-re" [GramInfo 2 1]
        checkMatchDictEntry set "ura" [GramInfo 2 1]
        checkMatchDictEntry set "tnt" [GramInfo 3 1]
        checkMatchDictEntry set "sta" [GramInfo 2 1]
        checkMatchDictEntry set "tnr" [GramInfo 3 1]
        checkMatchDictEntry set "rft" [GramInfo 3 1]
        checkMatchDictEntry set "tre" [GramInfo 0 1]
        checkMatchDictEntry set "ur"  [GramInfo 2 1]
        checkMatchDictEntry set "t,"  [GramInfo 3 1]
        checkMatchDictEntry set "t-"  [GramInfo 0 1, GramInfo 1 1, GramInfo 2 1, GramInfo 3 1]
        checkMatchDictEntry set "au"  [GramInfo 2 1]
        checkMatchDictEntry set "-te" [GramInfo 1 1]

    describe "FuzzySet 3 4 True mempty mempty mempty `add` ..." $
      let set = FuzzySet 3 4 True mempty mempty mempty
                            `add` "Trent"
                            `add` "pants"
                            `add` "restaurant"
                            `add` "XrF,!TNrATaNTNTNT"
       in do
        checkExactSet set
          [ ("trent"             , "Trent")
          , ("pants"             , "pants")
          , ("restaurant"        , "restaurant")
          , ("xrf,!tnratantntnt" , "XrF,!TNrATaNTNTNT") ]
        checkMagnitude set 3 0 2.23606797749979
        checkMagnitude set 3 1 2.23606797749979
        checkMagnitude set 3 2 3.1622776601683795
        checkMagnitude set 3 3 4.47213595499958
        checkMagnitude set 4 0 2.0
        checkMagnitude set 4 1 2.0
        checkMagnitude set 4 2 3.0
        checkMagnitude set 4 3 4.123105625617661
        checkMatchDictEntry set "ntnt" [GramInfo 3 2]
        checkMatchDictEntry set "tau"  [GramInfo 2 1]
        checkMatchDictEntry set "xrf"  [GramInfo 3 1]
        checkMatchDictEntry set "esta" [GramInfo 2 1]
        checkMatchDictEntry set "-pa"  [GramInfo 1 1]
        checkMatchDictEntry set "ran"  [GramInfo 2 1]
        checkMatchDictEntry set "ntn"  [GramInfo 3 2]
        checkMatchDictEntry set "-xr"  [GramInfo 3 1]
        checkMatchDictEntry set "ants" [GramInfo 1 1]
        checkMatchDictEntry set "-xrf" [GramInfo 3 1]
        checkMatchDictEntry set "ant"  [GramInfo 1 1, GramInfo 2 1, GramInfo 3 1]
        checkMatchDictEntry set "rant" [GramInfo 2 1]
        checkMatchDictEntry set "rat"  [GramInfo 3 1]
        checkMatchDictEntry set "antn" [GramInfo 3 1]
        checkMatchDictEntry set "nt-"  [GramInfo 0 1, GramInfo 2 1, GramInfo 3 1]
        checkMatchDictEntry set "rent" [GramInfo 0 1]
        checkMatchDictEntry set "rata" [GramInfo 3 1]
        checkMatchDictEntry set "tan"  [GramInfo 3 1]
        checkMatchDictEntry set "tant" [GramInfo 3 1]
        checkMatchDictEntry set "-tre" [GramInfo 0 1]
        checkMatchDictEntry set "est"  [GramInfo 2 1]
        checkMatchDictEntry set "-tr"  [GramInfo 0 1]

    describe "FuzzySet 2 5 True mempty mempty mempty `add` ..." $
      let set = FuzzySet 2 5 True mempty mempty mempty
                            `add` "Trent"
                            `add` "restaurant"
                            `add` "aunt"
                            `add` "Smarty Pants"
                            `add` "XrF,!TNrATaNTNTNT"
       in do
        checkExactSet set
          [ ("trent"             , "Trent")
          , ("restaurant"        , "restaurant")
          , ("aunt"              , "aunt")
          , ("smarty pants"      , "Smarty Pants")
          , ("xrf,!tnratantntnt" , "XrF,!TNrATaNTNTNT") ]
        checkMagnitude set 2 0 2.449489742783178
        checkMagnitude set 2 1 3.3166247903554
        checkMagnitude set 2 2 2.23606797749979
        checkMagnitude set 2 3 3.605551275463989
        checkMagnitude set 2 4 5.385164807134504
        checkMagnitude set 3 0 2.23606797749979
        checkMagnitude set 3 1 3.1622776601683795
        checkMagnitude set 3 2 2.0
        checkMagnitude set 3 3 3.4641016151377544
        checkMagnitude set 3 4 4.47213595499958
        checkMagnitude set 4 0 2.0
        checkMagnitude set 4 1 3.0
        checkMagnitude set 4 2 1.7320508075688772
        checkMagnitude set 4 3 3.3166247903554
        checkMagnitude set 4 4 4.123105625617661
        checkMagnitude set 5 0 1.7320508075688772
        checkMagnitude set 5 1 2.8284271247461903
        checkMagnitude set 5 2 1.4142135623730951
        checkMagnitude set 5 3 3.1622776601683795
        checkMagnitude set 5 4 3.7416573867739413
        checkMatchDictEntry set "pant"  [GramInfo 3 1]
        checkMatchDictEntry set "y "    [GramInfo 3 1]
        checkMatchDictEntry set "-xr"   [GramInfo 4 1]
        checkMatchDictEntry set "rest"  [GramInfo 1 1]
        checkMatchDictEntry set " p"    [GramInfo 3 1]
        checkMatchDictEntry set "ty p"  [GramInfo 3 1]
        checkMatchDictEntry set "rty"   [GramInfo 3 1]
        checkMatchDictEntry set "-tre"  [GramInfo 0 1]
        checkMatchDictEntry set "-a"    [GramInfo 2 1]
        checkMatchDictEntry set "ty"    [GramInfo 3 1]
        checkMatchDictEntry set "tntnt" [GramInfo 4 1]
        checkMatchDictEntry set "tr"    [GramInfo 0 1]
        checkMatchDictEntry set "ts"    [GramInfo 3 1]
        checkMatchDictEntry set "aun"   [GramInfo 2 1]
        checkMatchDictEntry set "tn"    [GramInfo 4 3]
        checkMatchDictEntry set "-t"    [GramInfo 0 1]
        checkMatchDictEntry set "aur"   [GramInfo 1 1]
        checkMatchDictEntry set "-s"    [GramInfo 3 1]
        checkMatchDictEntry set "-r"    [GramInfo 1 1]
        checkMatchDictEntry set "rty"   [GramInfo 3 1]
        checkMatchDictEntry set "tnra"  [GramInfo 4 1]
        checkMatchDictEntry set "nt"    [GramInfo 0 1, GramInfo 1 1, GramInfo 2 1, GramInfo 3 1, GramInfo 4 3]

    describe "values (defaultSet `add` ...)" $ do
      let set = defaultSet `add` "Trent" `add` "restaurant"
                           `add` "aunt"  `add` "Smarty Pants"
                           `add` "XrF,!TNrATaNTNTNT"
      it "should contain the added elements" $ do
        values set `shouldContain` ["Trent"]
        values set `shouldContain` ["restaurant"]
        values set `shouldContain` ["aunt"]
        values set `shouldContain` ["Smarty Pants"]
        values set `shouldContain` ["XrF,!TNrATaNTNTNT"]

    describe "size (defaultSet `add` ...)" $ do
      let set = defaultSet `add` "Trent" `add` "restaurant"
                           `add` "aunt"  `add` "Smarty Pants"
                           `add` "XrF,!TNrATaNTNTNT"
      it "should be 5" $ size set `shouldBe` 5

    describe "isEmpty (defaultSet `add` ...)" $ do
      let set = defaultSet `add` "Trent" `add` "restaurant"
                           `add` "aunt"  `add` "Smarty Pants"
                           `add` "XrF,!TNrATaNTNTNT"
      it "should be False" $ isEmpty set `shouldBe` False

    describe "isEmpty defaultSet" $ do
      let set = defaultSet
      it "should be True" $ isEmpty set `shouldBe` True

    describe "get (defaultSet `add` \"xxx\")" $ do
      let set = defaultSet `add` "xxx"
      it "should return [(1, \"xxx\")]" $
        get set "xxx" `shouldBe` [(1, "xxx")]

    checkMatches testset_1 "ant" 3 [(0, 1), (1, 2), (2, 1), (3, 1)]
    checkMatches testset_1 "pant" 3 [(0, 1), (1, 2), (2, 1), (3, 2)]
    checkMatches testset_1 "pants" 3 [(1, 1), (3, 4)]
    checkMatches testset_1 "tre" 3 [(0, 2)]
    checkMatches testset_1 "xxx" 3 []
    checkMatches testset_1 "xxx" 2 []

testset_1 ∷ FuzzySet
testset_1 = defaultSet `add` "Trent" `add` "restaurant"
                       `add` "aunt"  `add` "Smarty Pants"
