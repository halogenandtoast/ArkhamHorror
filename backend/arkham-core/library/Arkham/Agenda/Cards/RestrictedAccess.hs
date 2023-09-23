module Arkham.Agenda.Cards.RestrictedAccess (
  RestrictedAccess (..),
  restrictedAccess,
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
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries

newtype RestrictedAccess = RestrictedAccess AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restrictedAccess :: AgendaCard RestrictedAccess
restrictedAccess =
  agenda (1, A) RestrictedAccess Cards.restrictedAccess (Static 5)

instance HasAbilities RestrictedAccess where
  getAbilities (RestrictedAccess x) =
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemySpawns Timing.When Anywhere
      $ enemyIs Enemies.huntingHorror
    | onSide A x
    ]

instance RunMessage RestrictedAccess where
  runMessage msg a@(RestrictedAccess attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mShadowSpawnedId <- selectOne $ treacheryIs Treacheries.shadowSpawned
      case mShadowSpawnedId of
        Just tid -> push $ PlaceResources (toAbilitySource attrs 1) (toTarget tid) 1
        Nothing -> do
          huntingHorror <- selectJust $ enemyIs Enemies.huntingHorror
          shadowSpawned <- getSetAsideCard Treacheries.shadowSpawned
          push $ AttachStoryTreacheryTo shadowSpawned (toTarget huntingHorror)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorrorId <- selectOne $ enemyIs Enemies.huntingHorror
      case mHuntingHorrorId of
        Just eid ->
          pushAll
            [ PlaceDoom (toSource attrs) (toTarget eid) 1
            , advanceAgendaDeck attrs
            ]
        Nothing ->
          push
            $ FindEncounterCard
              leadInvestigatorId
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard, FromVoid]
              (cardIs Enemies.huntingHorror)
      pure a
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- selectJust $ LocationWithTitle "Museum Halls"
      pushAll
        [ EnemySpawnFromVoid Nothing lid eid
        , advanceAgendaDeck attrs
        ]
      pure a
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- selectJust $ LocationWithTitle "Museum Halls"
      pushAll
        [ SpawnEnemyAt (EncounterCard ec) lid
        , advanceAgendaDeck attrs
        ]
      pure a
    _ -> RestrictedAccess <$> runMessage msg attrs
