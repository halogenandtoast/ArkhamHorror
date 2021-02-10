module Arkham.Types.Agenda.Cards.RestrictedAccess
  ( RestrictedAccess(..)
  , restrictedAccess
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card.EncounterCardMatcher

newtype RestrictedAccess = RestrictedAccess AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restrictedAccess :: RestrictedAccess
restrictedAccess = RestrictedAccess
  $ baseAttrs "02119" "Restricted Access" (Agenda 1 A) (Static 5)

instance HasActions env RestrictedAccess where
  getActions i window (RestrictedAccess x) = getActions i window x

instance HasModifiersFor env RestrictedAccess where
  getModifiersFor = noModifiersFor

instance AgendaRunner env => RunMessage env RestrictedAccess where
  runMessage msg a@(RestrictedAccess attrs@AgendaAttrs {..}) = case msg of
    EnemySpawn _ _ eid -> do
      cardCode <- getId @CardCode eid
      when (cardCode == CardCode "02141") $ do
        mShadowSpawnedId <- fmap unStoryTreacheryId <$> getId (CardCode "02142")
        shadowSpawned <- EncounterCard <$> genEncounterCard "02142"
        case mShadowSpawnedId of
          Just tid -> unshiftMessage $ PlaceResources (TreacheryTarget tid) 1
          Nothing -> unshiftMessage
            $ AttachStoryTreacheryTo shadowSpawned (EnemyTarget eid)
      pure a
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      mHuntingHorrorId <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      a <$ case mHuntingHorrorId of
        Just eid -> unshiftMessages
          [PlaceDoom (EnemyTarget eid) 1, NextAgenda agendaId "02120"]
        Nothing -> unshiftMessage $ FindEncounterCard
          leadInvestigatorId
          (toTarget attrs)
          (EncounterCardMatchByCardCode "02141")
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- fromJustNote "Museum Halls missing"
        <$> getLocationIdWithTitle "Museum Halls"
      a <$ unshiftMessages
        [EnemySpawnFromVoid Nothing lid eid, NextAgenda agendaId "02120"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- fromJustNote "Museum Halls missing"
        <$> getLocationIdWithTitle "Museum Halls"
      a <$ unshiftMessages
        [SpawnEnemyAt (EncounterCard ec) lid, NextAgenda agendaId "02120"]
    _ -> RestrictedAccess <$> runMessage msg attrs
