module Arkham.Types.Asset.Uses where

import ClassyPrelude
import Data.Aeson

data UseType = Ammo | Supply | Secret | Charge | Try | Bounty | Whistle | Resource | Key
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Uses = NoUses | Uses UseType Int
  deriving stock (Show, Eq, Generic)

instance ToJSON Uses where
  toJSON NoUses = Null
  toJSON (Uses t n) = object ["type" .= toJSON t, "amount" .= toJSON n]
  toEncoding NoUses = toEncoding Null
  toEncoding (Uses t n) = pairs ("type" .= t <> "amount" .= n)

instance FromJSON Uses where
  parseJSON = \case
    Null -> pure NoUses
    Object o -> Uses <$> o .: "type" <*> o .: "amount"
    _ -> error "no such parse"
