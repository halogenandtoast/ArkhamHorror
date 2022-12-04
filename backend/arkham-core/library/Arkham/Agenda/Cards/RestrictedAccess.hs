module Arkham.Agenda.Cards.RestrictedAccess
  ( RestrictedAccess(..)
  , restrictedAccess
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
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Arkham.Zone

newtype RestrictedAccess = RestrictedAccess AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restrictedAccess :: AgendaCard RestrictedAccess
restrictedAccess =
  agenda (1, A) RestrictedAccess Cards.restrictedAccess (Static 5)

instance HasAbilities RestrictedAccess where
  getAbilities (RestrictedAccess x) =
    [ mkAbility x 1 $ ForcedAbility $ EnemySpawns Timing.When Anywhere $ enemyIs
        Enemies.huntingHorror
    ]

instance RunMessage RestrictedAccess where
  runMessage msg a@(RestrictedAccess attrs) = case msg of
    UseCardAbility _ source 1 [Window _ (Window.EnemySpawns eid _)] _
      | isSource attrs source -> do
        mShadowSpawnedId <- selectOne $ treacheryIs Treacheries.shadowSpawned
        case mShadowSpawnedId of
          Just tid -> push $ PlaceResources (TreacheryTarget tid) 1
          Nothing -> do
            shadowSpawned <- getSetAsideCard Treacheries.shadowSpawned
            push $ AttachStoryTreacheryTo shadowSpawned (EnemyTarget eid)
        pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorrorId <- selectOne $ enemyIs Enemies.huntingHorror
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
    _ -> RestrictedAccess <$> runMessage msg attrs
