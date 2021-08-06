module Arkham.Types.Card.CardCode where

import ClassyPrelude
import Data.Aeson
import qualified Data.Text as T

newtype CardCode = CardCode { unCardCode :: Text }
  deriving newtype (Ord, Show, Eq, Hashable, IsString)

instance ToJSON CardCode where
  toJSON (CardCode text) = toJSON $ cons 'c' text

instance FromJSON CardCode where
  parseJSON = withText "CardCode"
    $ \t -> pure $ CardCode $ if T.take 1 t == "c" then T.drop 1 t else t

instance ToJSONKey CardCode
instance FromJSONKey CardCode

newtype LocationCardCode = LocationCardCode { unLocationCardCode :: CardCode }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype ResignedCardCode = ResignedCardCode { unResignedCardCode :: CardCode }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype CommittedCardCode = CommittedCardCode { unCommittedCardCode :: CardCode }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype TreacheryCardCode = TreacheryCardCode { unTreacheryCardCode :: CardCode }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype VictoryDisplayCardCode = VictoryDisplayCardCode { unVictoryDisplayCardCode :: CardCode }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

class HasCardCode a where
  toCardCode :: a -> CardCode

instance HasCardCode CardCode where
  toCardCode = id
