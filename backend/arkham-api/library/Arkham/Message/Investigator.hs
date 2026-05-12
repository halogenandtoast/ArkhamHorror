{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Investigator where

import Arkham.Card (Card, EncounterCard, PlayerCard)
import Arkham.Card.Id (CardId)
import Arkham.Cost.Status (ActionStatus)
import Arkham.Strategy (DamageStrategy)
import Arkham.Deck (DeckSignifier)
import Arkham.Id
import Arkham.Matcher (AssetMatcher)
import {-# SOURCE #-} Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Slot (Slot, SlotType)
import Arkham.Source (Source)
import Arkham.Target (Target)
import Arkham.Window (Window)
import Arkham.Zone (Zone)
import Data.Aeson.TH

-- | Messages that are inherently scoped to one investigator (or to all
-- investigators): the damage-assignment flow, defeat/eliminate/kill cascade,
-- mulligan, resign, devour, clue spending/placing, asset-slot management,
-- card-draw notifications, and investigator-played-event signalling.
--
-- "Arkham.Message" exposes bidirectional pattern synonyms preserving the
-- pre-extraction public names; the underscore-suffixed constructors here are
-- the wrapped form.
data InvestigatorMessage
  = InvestigatorAssignDamage_ InvestigatorId Source DamageStrategy Int Int
  | InvestigatorCommittedCard_ InvestigatorId Card
  | InvestigatorCommittedSkill_ InvestigatorId SkillId
  | InvestigatorDamage_ InvestigatorId Source Int Int
  | InvestigatorDamageEnemy_ InvestigatorId EnemyId Source
  | InvestigatorDamageInvestigator_ InvestigatorId InvestigatorId
  | InvestigatorDefeated_ Source InvestigatorId
  | InvestigatorIsDefeated_ Source InvestigatorId
  | InvestigatorDirectDamage_ InvestigatorId Source Int Int
  | InvestigatorDiscardAllClues_ Source InvestigatorId
  | InvestigatorDoAssignDamage_ InvestigatorId Source DamageStrategy AssetMatcher Int Int [Target] [Target]
  | InvestigatorDrawEnemy_ InvestigatorId EnemyId
  | InvestigatorDrewEncounterCard_ InvestigatorId EncounterCard
  | InvestigatorDrewEncounterCardFrom_ InvestigatorId EncounterCard (Maybe DeckSignifier)
  | InvestigatorDrewPlayerCardFrom_ InvestigatorId PlayerCard (Maybe DeckSignifier)
  | InvestigatorEliminated_ InvestigatorId
  | InvestigatorKilled_ Source InvestigatorId
  | InvestigatorMulligan_ InvestigatorId
  | InvestigatorsMulligan_
  | InvestigatorPlaceAllCluesOnLocation_ InvestigatorId Source
  | InvestigatorPlaceCluesOnLocation_ InvestigatorId Source Int
  | InvestigatorPlayAsset_ InvestigatorId AssetId
  | InvestigatorClearUnusedAssetSlots_ InvestigatorId [AssetId]
  | InvestigatorAdjustAssetSlots_ InvestigatorId AssetId
  | InvestigatorAdjustSlot_ InvestigatorId Slot SlotType SlotType
  | InvestigatorPlayedAsset_ InvestigatorId AssetId
  | InvestigatorPlayEvent_ InvestigatorId EventId (Maybe Target) [Window] Zone
  | InvestigatorResigned_ InvestigatorId
  | InvestigatorSpendClues_ InvestigatorId Int
  | InvestigatorWhenDefeated_ Source InvestigatorId
  | InvestigatorWhenEliminated_ Source InvestigatorId (Maybe Message)
  | InvestigatorSpecific_ InvestigatorId Text Value
  | HandleKilledOrInsaneInvestigators_
  | Resign_ InvestigatorId
  | ResignWith_ Target
  | Devour_ InvestigatorId
  | Devoured_ InvestigatorId Card
  | BeforePlayEvent_ InvestigatorId EventId ActiveCostId
  | CreatePendingEvent_ Card InvestigatorId EventId
  | BeforeCardCost_ InvestigatorId ActionStatus [Window] CardId
  | FinishedEvent_ EventId
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''InvestigatorMessage)
