module Recreation.ClientSpec (spec) where

import Control.Lens (view)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as BC8
import Recreation.Client (toCampsite)
import Recreation.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "decode campsite" $ do
    it "decodes valid JSON successfully" $ do
      jsonStr <- BC8.readFile "test/campsite1.json"
      let campsite = eitherDecodeStrict jsonStr >>= toCampsite
      fmap (view campsiteId) campsite `shouldBe` Right "2558"
      fmap (length . view availabilities) campsite `shouldBe` Right 7
    it "fails to decode invalid availability with an error" $ do
      jsonStr <- BC8.readFile "test/campsite_invalid_1.json"
      let campsite = eitherDecodeStrict jsonStr >>= toCampsite
      campsite `shouldBe` Left "failed to parse 2023-05-T00:00:00Z"
