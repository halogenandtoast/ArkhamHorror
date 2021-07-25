module Arkham.Types.Card.PlayerCard where

import Arkham.Prelude

import Arkham.Json
import Arkham.PlayerCard
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.Class
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.InvestigatorId

newtype BearerId = BearerId { unBearerId :: InvestigatorId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype DiscardedPlayerCard = DiscardedPlayerCard { unDiscardedPlayerCard :: PlayerCard }

data PlayerCard = MkPlayerCard
  { pcId :: CardId
  , pcBearer :: Maybe InvestigatorId
  , pcCardCode :: CardCode
  , pcOriginalCardCode :: CardCode
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

instance ToJSON PlayerCard where
  toJSON = genericToJSON $ aesonOptions $ Just "pc"
  toEncoding = genericToEncoding $ aesonOptions $ Just "pc"

instance FromJSON PlayerCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

instance HasSkillIcons PlayerCard where
  getSkillIcons = cdSkills . toCardDef

instance HasCost PlayerCard where
  getCost c = case cdCost (toCardDef c) of
    Just (StaticCost n) -> n
    Just DynamicCost -> 0
    Nothing -> 0

instance HasCardDef PlayerCard where
  toCardDef c = case lookup (pcCardCode c) allPlayerCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for player card " <> show (pcCardCode c)

instance HasOriginalCardCode PlayerCard where
  toOriginalCardCode = pcOriginalCardCode

lookupPlayerCard :: CardDef -> CardId -> PlayerCard
lookupPlayerCard cardDef cardId = MkPlayerCard
  { pcId = cardId
  , pcCardCode = toCardCode cardDef
  , pcOriginalCardCode = toCardCode cardDef
  , pcBearer = Nothing
  }

genPlayerCard :: MonadRandom m => CardDef -> m PlayerCard
genPlayerCard cardDef = lookupPlayerCard cardDef <$> getRandom
