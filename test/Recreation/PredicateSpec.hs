module Recreation.PredicateSpec (spec) where

import Data.Functor.Contravariant (getPredicate)
import Data.Time (DayOfWeek (Monday, Saturday, Tuesday), fromGregorian)
import Recreation.Predicate
import Recreation.Types.Campsite (Campsite (Campsite))
import qualified Recreation.Types.Campsite as Campsite
import Test.Hspec

spec :: Spec
spec = do
  describe "dayFrom" $ do
    it "accepts dates on or after the provided date" $ do
      let p = getPredicate . dayFrom $ fromGregorian 2023 02 05
      p (fromGregorian 2023 02 05) `shouldBe` True
      p (fromGregorian 2023 02 06) `shouldBe` True
      p (fromGregorian 2024 01 01) `shouldBe` True
      p (fromGregorian 2023 02 04) `shouldBe` False
  describe "dayUntil" $ do
    it "accepts dates on or before the provided date" $ do
      let p = getPredicate . dayUntil $ fromGregorian 2023 02 05
      p (fromGregorian 2023 02 05) `shouldBe` True
      p (fromGregorian 2023 02 06) `shouldBe` False
      p (fromGregorian 2024 01 01) `shouldBe` False
      p (fromGregorian 2023 02 04) `shouldBe` True
  describe "daysBetween" $ do
    it "accepts dates between the provided dates" $ do
      let p = getPredicate $ daysBetween (fromGregorian 2023 01 01) (fromGregorian 2023 01 15)
      p (fromGregorian 2022 12 31) `shouldBe` False
      p (fromGregorian 2023 01 01) `shouldBe` True
      p (fromGregorian 2023 01 15) `shouldBe` True
      p (fromGregorian 2023 01 16) `shouldBe` False
    it "accepts no dates if start date is after end date" $ do
      let p = getPredicate $ daysBetween (fromGregorian 2023 01 15) (fromGregorian 2023 01 01)
      p (fromGregorian 2023 01 01) `shouldBe` False
      p (fromGregorian 2023 01 02) `shouldBe` False
      p (fromGregorian 2023 01 15) `shouldBe` False
  describe "dayOfWeekIn" $ do
    it "accepts dates for which the day of week is in the provided list" $ do
      let p = getPredicate $ dayOfWeekIn [Monday, Saturday]
      p (fromGregorian 2024 03 04) `shouldBe` True
      p (fromGregorian 2024 03 05) `shouldBe` False
      p (fromGregorian 2024 03 09) `shouldBe` True
      p (fromGregorian 2024 03 10) `shouldBe` False
      p (fromGregorian 2024 03 11) `shouldBe` True
  describe "anyAvailableDayMatching" $ do
    it "accepts campsites for which there is an available date matching the date predicate" $ do
      let c = Campsite "" "" [(fromGregorian 2024 03 04, Campsite.Available)]
          p = getPredicate $ anyAvailableDayMatching $ dayOfWeekIn [Monday]
      p c `shouldBe` True
    it "rejects a campsite that has no available dates" $ do
      let c = Campsite "" "" [(fromGregorian 2024 03 04, Campsite.NotAvailable)]
          p = getPredicate $ anyAvailableDayMatching alwaysTrue
      p c `shouldBe` False
    it "rejects a campsite that has no available dates matching the date predicate" $ do
      let c = Campsite "" "" [(fromGregorian 2024 03 04, Campsite.Available)]
          p = getPredicate $ anyAvailableDayMatching $ dayOfWeekIn [Tuesday]
      p c `shouldBe` False
