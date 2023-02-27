module Arkham.Card.PlayerCard where

import Arkham.Prelude

import Arkham.Json
import Arkham.PlayerCard
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.Class
import Arkham.Card.Cost
import Arkham.Card.Id
import Arkham.Id
import Arkham.Name
import Arkham.SkillType

newtype DiscardedPlayerCard = DiscardedPlayerCard { unDiscardedPlayerCard :: PlayerCard }

data PlayerCard = MkPlayerCard
  { pcId :: CardId
  , pcOwner :: Maybe InvestigatorId
  , pcCardCode :: CardCode
  , pcOriginalCardCode :: CardCode
  , pcCustomizations :: IntMap Int
  }
  deriving stock (Show, Generic)
  deriving anyclass Hashable

instance Eq PlayerCard where
  pc1 == pc2 = pcId pc1 == pcId pc2

instance ToJSON PlayerCard where
  toJSON = genericToJSON $ aesonOptions $ Just "pc"
  toEncoding = genericToEncoding $ aesonOptions $ Just "pc"

instance FromJSON PlayerCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

instance HasCardCode PlayerCard where
  toCardCode = pcCardCode

instance HasSkills PlayerCard where
  toSkills = withCardDef toSkills

instance HasCost PlayerCard where
  getCost c = case withCardDef cdCost (toCardDef c) of
    Just (StaticCost n) -> n
    Just DynamicCost -> 0
    Nothing -> 0

instance HasCardDef PlayerCard where
  toCardDef c = case lookup (pcCardCode c) allPlayerCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for player card " <> show (pcCardCode c)

instance Named PlayerCard where
  toName = withCardDef toName

instance HasOriginalCardCode PlayerCard where
  toOriginalCardCode = pcOriginalCardCode

lookupPlayerCard :: HasCardCode a => a -> CardId -> PlayerCard
lookupPlayerCard cardDef cardId = MkPlayerCard
  { pcId = cardId
  , pcCardCode = toCardCode cardDef
  , pcOriginalCardCode = toCardCode cardDef
  , pcOwner = Nothing
  , pcCustomizations = mempty
  }
