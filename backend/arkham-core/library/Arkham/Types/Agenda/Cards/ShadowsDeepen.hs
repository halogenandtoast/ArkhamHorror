module Arkham.Types.Agenda.Cards.ShadowsDeepen
  ( ShadowsDeepen(..)
  , shadowsDeepen
  ) where

import Arkham.Prelude

import Arkham.EncounterCard
import qualified Arkham.Treachery.Cards as Treacheries
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.TreacheryId

newtype ShadowsDeepen = ShadowsDeepen AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowsDeepen :: ShadowsDeepen
shadowsDeepen =
  ShadowsDeepen $ baseAttrs "02120" "Shadows Deepen" (Agenda 2 A) (Static 7)

instance HasActions env ShadowsDeepen where
  getActions i window (ShadowsDeepen x) = getActions i window x

instance HasModifiersFor env ShadowsDeepen

instance AgendaRunner env => RunMessage env ShadowsDeepen where
  runMessage msg a@(ShadowsDeepen attrs@AgendaAttrs {..}) = case msg of
    EnemySpawn _ _ eid -> do
      cardCode <- getId @CardCode eid
      when (cardCode == CardCode "02141") $ do
        mShadowSpawnedId <- fmap unStoryTreacheryId
          <$> getId (toCardCode Treacheries.shadowSpawned)
        shadowSpawned <- EncounterCard
          <$> genEncounterCard Treacheries.shadowSpawned
        case mShadowSpawnedId of
          Just tid -> push $ PlaceResources (TreacheryTarget tid) 1
          Nothing ->
            push $ AttachStoryTreacheryTo shadowSpawned (EnemyTarget eid)
      pure a
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      mHuntingHorrorId <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      a <$ case mHuntingHorrorId of
        Just eid ->
          pushAll [PlaceDoom (EnemyTarget eid) 1, NextAgenda agendaId "02121"]
        Nothing -> push $ FindEncounterCard
          leadInvestigatorId
          (toTarget attrs)
          (CardMatchByCardCode "02141")
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- fromJustNote "Museum Halls missing"
        <$> getLocationIdWithTitle "Museum Halls"
      a <$ pushAll
        [EnemySpawnFromVoid Nothing lid eid, NextAgenda agendaId "02121"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- fromJustNote "Museum Halls missing"
        <$> getLocationIdWithTitle "Museum Halls"
      a <$ pushAll
        [SpawnEnemyAt (EncounterCard ec) lid, NextAgenda agendaId "02121"]
    _ -> ShadowsDeepen <$> runMessage msg attrs
