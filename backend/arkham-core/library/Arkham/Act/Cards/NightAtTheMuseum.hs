module Arkham.Act.Cards.NightAtTheMuseum (
  NightAtTheMuseum (..),
  nightAtTheMuseum,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Timing qualified as Timing

newtype NightAtTheMuseum = NightAtTheMuseum ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightAtTheMuseum :: ActCard NightAtTheMuseum
nightAtTheMuseum = act (2, A) NightAtTheMuseum Cards.nightAtTheMuseum Nothing

instance HasAbilities NightAtTheMuseum where
  getAbilities (NightAtTheMuseum x) =
    [ mkAbility x 1 $
        ForcedAbility $
          Enters Timing.When You $
            locationIs
              Cards.exhibitHallRestrictedHall
    ]

instance RunMessage NightAtTheMuseum where
  runMessage msg a@(NightAtTheMuseum attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorror <- getHuntingHorror
      case mHuntingHorror of
        Just eid -> do
          lid <- getRestrictedHall
          pushAll
            [ EnemySpawn Nothing lid eid
            , Ready (EnemyTarget eid)
            , AdvanceActDeck (actDeckId attrs) (toSource attrs)
            ]
        Nothing ->
          push $
            FindEncounterCard
              leadInvestigatorId
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard, FromVoid]
              (cardIs Enemies.huntingHorror)
      pure a
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- getRestrictedHall
      pushAll
        [ EnemySpawnFromVoid Nothing lid eid
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- getRestrictedHall
      pushAll
        [ SpawnEnemyAt (EncounterCard ec) lid
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> NightAtTheMuseum <$> runMessage msg attrs
