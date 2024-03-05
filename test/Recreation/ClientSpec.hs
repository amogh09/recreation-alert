module Recreation.ClientSpec (spec) where

import Control.Exception (throw)
import Control.Lens (view)
import Data.Aeson (decodeStrict, eitherDecodeStrict)
import qualified Data.ByteString.Char8 as BC8
import Data.Maybe (fromJust)
import Data.Time.Calendar (fromGregorian)
import Recreation.Client.Internal (toCampsite)
import Recreation.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "toCampsite" $ do
    it "decodes valid JSON successfully" $ do
      jsonStr <- BC8.readFile "test/campsite1.json"
      let campsite =
            either (throw . stringException) Prelude.id
              . toCampsite
              . fromJust
              . decodeStrict
              $ jsonStr
      view campsiteId campsite `shouldBe` "2558"
      (length . view availabilities) campsite `shouldBe` 7
      (fmap fst . filter ((== Available) . snd) . view availabilities) campsite
        `shouldBe` [fromGregorian 2023 5 26, fromGregorian 2023 5 28]
    it "fails to decode invalid availability with an error" $ do
      jsonStr <- BC8.readFile "test/campsite_invalid_1.json"
      let campsite = eitherDecodeStrict jsonStr >>= toCampsite
      campsite `shouldBe` Left "failed to parse 2023-05-T00:00:00Z"
