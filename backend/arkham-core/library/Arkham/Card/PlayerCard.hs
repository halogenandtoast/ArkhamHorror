{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card.PlayerCard where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.Class
import Arkham.Card.Cost
import Arkham.Card.Id
import Arkham.Enemy.Cards (allSpecialEnemyCards)
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.PlayerCard
import Data.Aeson.TH

data PlayerCard = MkPlayerCard
  { pcId :: CardId
  , pcOwner :: Maybe InvestigatorId
  , pcCardCode :: CardCode
  , pcOriginalCardCode :: CardCode
  , pcCustomizations :: IntMap Int
  }
  deriving stock (Show, Ord, Data)

instance Eq PlayerCard where
  pc1 == pc2 = pcId pc1 == pcId pc2

instance HasCardCode PlayerCard where
  toCardCode = pcCardCode

instance HasCost PlayerCard where
  getCost c = case cdCost (toCardDef c) of
    Just (StaticCost n) -> n
    Just DynamicCost -> 0
    Nothing -> 0

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
