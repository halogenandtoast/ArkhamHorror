module Arkham.Types.Card.CardCode where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Types
import Data.Text qualified as T

newtype CardCode = CardCode { unCardCode :: Text }
  deriving newtype (Show, Eq, Hashable, IsString)

instance ToJSON CardCode where
  toJSON = toJSON . T.cons 'c' . unCardCode

instance FromJSON CardCode where
  parseJSON =
    withText "CardCode" $ \t -> pure $ CardCode $ T.dropWhile (== 'c') t

instance ToJSONKey CardCode where
  toJSONKey = toJSONKeyText (T.cons 'c' . unCardCode)

instance FromJSONKey CardCode where
  fromJSONKey = CardCode . T.dropWhile (== 'c') <$> fromJSONKey

newtype LocationCardCode = LocationCardCode { unLocationCardCode :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype ResignedCardCode = ResignedCardCode { unResignedCardCode :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype CommittedCardCode = CommittedCardCode { unCommittedCardCode :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype TreacheryCardCode = TreacheryCardCode { unTreacheryCardCode :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype VictoryDisplayCardCode = VictoryDisplayCardCode { unVictoryDisplayCardCode :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

class HasCardCode a where
  toCardCode :: a -> CardCode

instance HasCardCode CardCode where
  toCardCode = id
