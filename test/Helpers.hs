module Helpers
  ( shouldBeTrue
  , shouldBeIn
  , shouldBeCloseTo
  , shouldNotBeCloseTo
  ) where

import Data.AEq
import Test.Hspec

shouldBeTrue :: Bool -> Expectation
shouldBeTrue = shouldBe True

shouldBeIn :: Eq a => a -> [a] -> Expectation
shouldBeIn x xs = shouldBeTrue (x `elem` xs)

shouldBeCloseTo :: Double -> Double -> Expectation
shouldBeCloseTo q r = shouldBeTrue (q ~== r)

shouldNotBeCloseTo :: Double -> Double -> Expectation
shouldNotBeCloseTo q r = shouldBeTrue $ not (q ~== r)
