{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Recreation.Client (toCampsite, ApiCampsite) where

import Data.Aeson (FromJSON)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatParseM, iso8601ParseM)
import GHC.Generics (Generic)
import Recreation.Types

data ApiCampsite = ApiCampsite
  { campsite_id :: !String,
    site :: !String,
    availabilities :: Map String String
  }
  deriving (Show, Generic)

type ApiCampsites = Map String ApiCampsite

instance FromJSON ApiCampsite

toCampsite :: ApiCampsite -> Either String Campsite
toCampsite ac =
  Campsite
    <$> pure ac.campsite_id
    <*> pure ac.site
    <*> ( mapM tupleEither
            . fmap
              (bimap parseDay parseAvailability)
            $ Map.toList ac.availabilities
        )

parseDay :: String -> Either String Day
parseDay x =
  fmap utctDay
    . maybe (Left $ "failed to parse " <> x) Right
    . iso8601ParseM
    $ x

parseAvailability :: String -> Either String Availability
parseAvailability "Available" = Right Available
parseAvailability _ = Right NotAvailable

tupleEither :: (Either a b, Either a c) -> Either a (b, c)
tupleEither (Left e, _) = Left e
tupleEither (_, Left e) = Left e
tupleEither (Right x, Right y) = Right (x, y)
