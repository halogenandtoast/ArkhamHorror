module Arkham.Agenda.Cards.ShadowsDeepen
  ( ShadowsDeepen(..)
  , shadowsDeepen
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Arkham.Zone

newtype ShadowsDeepen = ShadowsDeepen AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowsDeepen :: AgendaCard ShadowsDeepen
shadowsDeepen = agenda (2, A) ShadowsDeepen Cards.shadowsDeepen (Static 7)

instance HasAbilities ShadowsDeepen where
  getAbilities (ShadowsDeepen x) =
    [ mkAbility x 1 $ ForcedAbility $ EnemySpawns Timing.When Anywhere $ enemyIs
        Enemies.huntingHorror
    ]

instance RunMessage ShadowsDeepen where
  runMessage msg a@(ShadowsDeepen attrs) = case msg of
    UseCardAbility _ source 1 [Window _ (Window.EnemySpawns eid _)] _
      | isSource attrs source -> do
        mShadowSpawnedId <- selectOne $ treacheryIs Treacheries.shadowSpawned
        shadowSpawned <- EncounterCard
          <$> genEncounterCard Treacheries.shadowSpawned
        case mShadowSpawnedId of
          Just tid -> push $ PlaceResources (TreacheryTarget tid) 1
          Nothing ->
            push $ AttachStoryTreacheryTo shadowSpawned (EnemyTarget eid)
        pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorrorId <- getHuntingHorror
      case mHuntingHorrorId of
        Just eid -> pushAll
          [ PlaceDoom (EnemyTarget eid) 1
          , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
          ]
        Nothing -> push $ FindEncounterCard
          leadInvestigatorId
          (toTarget attrs)
          [FromEncounterDeck, FromEncounterDiscard, FromVoid]
          (cardIs Enemies.huntingHorror)
      pure a
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- selectJust $ LocationWithTitle "Museum Halls"
      pushAll
        [ EnemySpawnFromVoid Nothing lid eid
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- selectJust $ LocationWithTitle "Museum Halls"
      pushAll
        [ SpawnEnemyAt (EncounterCard ec) lid
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> ShadowsDeepen <$> runMessage msg attrs
