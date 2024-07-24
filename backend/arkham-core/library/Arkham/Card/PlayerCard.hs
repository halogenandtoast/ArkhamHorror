{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card.PlayerCard where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.Class
import Arkham.Card.Cost
import Arkham.Card.Id
import Arkham.Customization
import Arkham.Enemy.Cards (allSpecialEnemyCards)
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.PlayerCard
import Arkham.SkillType
import Data.Aeson.TH
import GHC.Records

data PlayerCard = MkPlayerCard
  { pcId :: CardId
  , pcOwner :: Maybe InvestigatorId
  , pcCardCode :: CardCode
  , pcOriginalCardCode :: CardCode
  , pcCustomizations :: Customizations
  }
  deriving stock (Show, Ord, Data)

instance HasField "id" PlayerCard CardId where
  getField = pcId

instance HasField "customizations" PlayerCard Customizations where
  getField = pcCustomizations

instance HasField "owner" PlayerCard (Maybe InvestigatorId) where
  getField = pcOwner

instance Eq PlayerCard where
  pc1 == pc2 = pcId pc1 == pcId pc2

instance HasCardCode PlayerCard where
  toCardCode = pcCardCode

instance HasCost PlayerCard where
  getCost c = case cdCost (toCardDef c) of
    Just (StaticCost n) -> n
    Just DynamicCost -> 0
    Just (MaxDynamicCost _) -> 0
    Just DiscardAmountCost -> 0
    Nothing -> 0

instance HasField "skills" PlayerCard [SkillIcon] where
  getField = cdSkills . toCardDef

instance HasCardDef PlayerCard where
  toCardDef c = case lookup (pcCardCode c) (allPlayerCards <> allSpecialEnemyCards) of
    Just def -> def
    Nothing ->
      error $ "missing card def for player card " <> show (pcCardCode c)

instance Named PlayerCard where
  toName = toName . toCardDef

instance HasOriginalCardCode PlayerCard where
  toOriginalCardCode = pcOriginalCardCode

lookupPlayerCard :: CardDef -> CardId -> PlayerCard
lookupPlayerCard cardDef cardId =
  MkPlayerCard
    { pcId = cardId
    , pcCardCode = toCardCode cardDef
    , pcOriginalCardCode = toCardCode cardDef
    , pcOwner = Nothing
    , pcCustomizations = mempty
    }

setPlayerCardOwner :: InvestigatorId -> PlayerCard -> PlayerCard
setPlayerCardOwner iid pc = pc {pcOwner = Just iid}

$(deriveJSON (aesonOptions $ Just "pc") ''PlayerCard)
