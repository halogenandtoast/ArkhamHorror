module Arkham.Types.Agenda.Cards.RestrictedAccess
  ( RestrictedAccess(..)
  , restrictedAccess
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype RestrictedAccess = RestrictedAccess AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restrictedAccess :: AgendaCard RestrictedAccess
restrictedAccess =
  agenda (1, A) RestrictedAccess Cards.restrictedAccess (Static 5)

instance HasAbilities RestrictedAccess where
  getAbilities (RestrictedAccess x) =
    [ mkAbility x 1 $ ForcedAbility $ EnemySpawns Timing.When Anywhere $ enemyIs
        Cards.huntingHorror
    ]

instance AgendaRunner env => RunMessage env RestrictedAccess where
  runMessage msg a@(RestrictedAccess attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.EnemySpawns eid _)] 1 _
      | isSource attrs source -> do
        mShadowSpawnedId <- fmap unStoryTreacheryId
          <$> getId (toCardCode Treacheries.shadowSpawned)
        a <$ case mShadowSpawnedId of
          Just tid -> push $ PlaceResources (TreacheryTarget tid) 1
          Nothing -> do
            shadowSpawned <- getSetAsideCard Treacheries.shadowSpawned
            push $ AttachStoryTreacheryTo shadowSpawned (EnemyTarget eid)
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorrorId <- selectOne $ enemyIs Cards.huntingHorror
      a <$ case mHuntingHorrorId of
        Just eid -> pushAll
          [PlaceDoom (EnemyTarget eid) 1, NextAgenda (toId attrs) "02120"]
        Nothing -> push $ FindEncounterCard
          leadInvestigatorId
          (toTarget attrs)
          (CardWithCardCode "02141")
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- fromJustNote "Museum Halls missing"
        <$> selectOne (LocationWithTitle "Museum Halls")
      a <$ pushAll
        [EnemySpawnFromVoid Nothing lid eid, NextAgenda (toId attrs) "02120"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- fromJustNote "Museum Halls missing"
        <$> selectOne (LocationWithTitle "Museum Halls")
      a <$ pushAll
        [SpawnEnemyAt (EncounterCard ec) lid, NextAgenda (toId attrs) "02120"]
    _ -> RestrictedAccess <$> runMessage msg attrs
