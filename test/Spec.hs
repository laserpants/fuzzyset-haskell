{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Control.Exception             ( evaluate )
import Control.Lens
import Data.AEq
import Data.FuzzySet
import Data.FuzzySet.Lens
import Data.HashMap.Strict           ( HashMap, fromList )
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

gramsCount ∷ Text → Size → SpecWith ()
gramsCount input n = it message $ do
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

main ∷ IO ()
main = hspec $ do

    describe "grams" $ do
      mapM_ (gramsCount "charade") [2..6]
      it "should throw an error if n < 2" $
        evaluate (grams "anything" 1) `shouldThrow` anyException

    describe "grams \"charade\" 2" $
      it "should return [\"-c\", \"ch\", \"ha\", \"ar\", \"ra\", \"ad\", \"de\", \"e-\"]" $
        grams "charade" 2 `shouldBe` ["-c", "ch", "ha", "ar", "ra", "ad", "de", "e-"]

    describe "grams \"chArade\" 2" $
      it "should return [\"-c\", \"ch\", \"ha\", \"ar\", \"ra\", \"ad\", \"de\", \"e-\"]" $
        grams "charade" 2 `shouldBe` ["-c", "ch", "ha", "ar", "ra", "ad", "de", "e-"]

    describe "grams \"charade\" 3" $
      it "should return [\"-ch\", \"cha\", \"har\", \"ara\", \"rad\", \"ade\", \"de-\"]" $
        grams "charade" 3 `shouldBe` ["-ch", "cha", "har", "ara", "rad", "ade", "de-"]

    describe "grams \"aFl1pP!.,nG FL0^ppy+\" 2" $
      it "should return [\"-a\", \"af\", \"fl\", \"l1\", \"1p\", \"pp\", \"p,\", \",n\", \"ng\", \"g \", \" f\", \"fl\", \"l0\", \"0p\", \"pp\", \"py\", \"y-\"]" $
        grams "aFl1pP!.,nG FL0^ppy+" 2 `shouldBe` ["-a", "af", "fl", "l1", "1p", "pp", "p,", ",n", "ng", "g ", " f", "fl", "l0", "0p", "pp", "py", "y-"]

    describe "gramMap \"xxx\" 2" $
      it "should return fromList [(\"-x\", 1),(\"xx\", 2),(\"x-\", 1)]" $
        gramMap "xxx" 2 `shouldBe` fromList [("-x", 1),("xx", 2),("x-", 1)]

    describe "gramMap \"xxx\" 3" $
      it "should return fromList [(\"-xx\", 1), (\"xx-\", 1), (\"xxx\", 1)]" $
        gramMap "xxx" 3 `shouldBe` fromList [("-xx", 1), ("xx-", 1), ("xxx", 1)]

    describe "gramMap \"xxxxxxx\" 4" $
      it "should return [(\"-xxx\", 1), (\"xxxx\", 4), (\"xxx-\", 1)]" $
        gramMap "xxxxxxx" 4 `shouldBe` fromList [("-xxx", 1), ("xxxx", 4), ("xxx-", 1)]

    describe "gramMap \"bananasananas\" 2" $
      it "should return fromList [(\"-b\", 1), (\"ba\", 1), (\"an\", 4), (\"na\", 4), (\"as\", 2), (\"sa\", 1), (\"s-\", 1)]" $
        gramMap "bananasananas" 2 `shouldBe`
          fromList [("-b", 1), ("ba", 1), ("an", 4), ("na", 4), ("as", 2), ("sa", 1), ("s-", 1)]

    describe "gramMap \"bananasananas\" 3" $
      it "should return [(\"-ba\", 1), (\"ban\", 1), (\"ana\", 4), (\"nan\", 2), (\"nas\", 2), (\"asa\", 1), (\"san\", 1), (\"as-\", 1)]" $
        gramMap "bananasananas" 3 `shouldBe`
          fromList [("-ba", 1), ("ban", 1), ("ana", 4), ("nan", 2), ("nas", 2), ("asa", 1), ("san", 1), ("as-", 1)]

    describe "gramMap \"trentsauntsrestaurant\" 2" $
      let grams = gramMap "trentsauntsrestaurant" 2
       in do
         checkMapKey grams "nt" 3
         checkMapKey grams "au" 2
         checkMapKey grams "ts" 2
         checkMapKey grams "re" 2
         checkMapKey grams "st" 1
         checkMapKey grams "en" 1

    describe "gramMap \"trentsauntsrestaurant\" 3" $
      let grams = gramMap "trentsauntsrestaurant" 3
       in do
         checkMapKey grams "res" 1
         checkMapKey grams "nts" 2

    describe "gramMap \"trentsantwantstorentpants\" 3" $
      let grams = gramMap "trentsantwantstorentpants" 3
       in do
         checkMapKey grams "pan" 1
         checkMapKey grams "twa" 1
         checkMapKey grams "ant" 3
         checkMapKey grams "ren" 2
         checkMapKey grams "ent" 2
         checkMapKey grams "nts" 3

    describe "gramMap \"trentsantwantstorentpantstostartrestaurant\" 3" $
      let grams = gramMap "trentsantwantstorentpantstostartrestaurant" 3
       in do
         checkMapKey grams "ant" 4
         checkMapKey grams "nts" 3
         checkMapKey grams "sto" 2
         checkMapKey grams "sta" 2
         checkMapKey grams "ren" 2
         checkMapKey grams "tre" 2

    describe "gramMap \"trentsantwantstorentpantstostartrestaurant\" 2" $
      let grams = gramMap "trentsantwantstorentpantstostartrestaurant" 2
       in do
         checkMapKey grams "an" 4
         checkMapKey grams "st" 4
         checkMapKey grams "re" 3
         checkMapKey grams "ts" 3
         checkMapKey grams "en" 2
         checkMapKey grams "to" 2
         checkMapKey grams "tr" 2
         checkMapKey grams "or" 1
         checkMapKey grams "au" 1
         checkMapKey grams "ur" 1

    describe "gramMap \"antsintrentspantswanttrentsauntsrestaurant\" 3" $
      let grams = gramMap "antsintrentspantswanttrentsauntsrestaurant" 3
       in do
         checkMapKey grams "nts" 5
         checkMapKey grams "ant" 4
         checkMapKey grams "ent" 2

    describe "gramMap \"asmartantintrentspantswantstorenttrentsauntsrestaurant\" 3" $
      let grams = gramMap "asmartantintrentspantswantstorenttrentsauntsrestaurant" 3
       in do
         checkMapKey grams "nts" 5
         checkMapKey grams "ant" 4
         checkMapKey grams "ent" 3

    describe "gramMap \"buffalo buffalo buffalo buffalo buffalo buffalo\" 7" $
      let grams = gramMap "buffalo buffalo buffalo buffalo buffalo buffalo" 7
       in checkMapKey grams "buffalo" 6

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
