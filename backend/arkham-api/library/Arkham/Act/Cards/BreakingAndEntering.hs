module Arkham.Act.Cards.BreakingAndEntering (
  BreakingAndEntering (..),
  breakingAndEntering,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers

newtype BreakingAndEntering = BreakingAndEntering ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingAndEntering :: ActCard BreakingAndEntering
breakingAndEntering =
  act (2, A) BreakingAndEntering Cards.breakingAndEntering Nothing

instance HasAbilities BreakingAndEntering where
  getAbilities (BreakingAndEntering x) =
    [mkAbility x 1 $ forced $ Enters #after You $ locationIs Cards.exhibitHallRestrictedHall]

instance RunMessage BreakingAndEntering where
  runMessage msg a@(BreakingAndEntering attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) source AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      (leadInvestigatorId, lead) <- getLeadInvestigatorPlayer
      investigatorIds <- getInvestigators
      mHuntingHorror <- getHuntingHorror
      haroldWalsted <- getSetAsideCard Assets.haroldWalsted
      case mHuntingHorror of
        Just eid -> do
          lid <- getRestrictedHall
          pushAll
            [ chooseOne
                lead
                [ targetLabel iid [TakeControlOfSetAsideAsset iid haroldWalsted]
                | iid <- investigatorIds
                ]
            , EnemySpawn Nothing lid eid
            , Ready (EnemyTarget eid)
            , AdvanceActDeck (actDeckId attrs) (toSource attrs)
            ]
        Nothing ->
          pushAll
            [ chooseOne
                lead
                [ targetLabel iid [TakeControlOfSetAsideAsset iid haroldWalsted]
                | iid <- investigatorIds
                ]
            , FindEncounterCard
                leadInvestigatorId
                (toTarget attrs)
                [FromEncounterDeck, FromEncounterDiscard, FromVoid]
                (cardIs Enemies.huntingHorror)
            ]
      pure a
    FoundEnemyInOutOfPlay VoidZone _ target eid | isTarget attrs target -> do
      lid <- getRestrictedHall
      pushAll
        [ EnemySpawnFromOutOfPlay VoidZone Nothing lid eid
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
    _ -> BreakingAndEntering <$> runMessage msg attrs
