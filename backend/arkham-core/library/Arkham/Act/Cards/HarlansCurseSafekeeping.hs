module Arkham.Act.Cards.HarlansCurseSafekeeping
  ( HarlansCurseSafekeeping(..)
  , harlansCurseSafekeeping
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype HarlansCurseSafekeeping = HarlansCurseSafekeeping ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harlansCurseSafekeeping :: ActCard HarlansCurseSafekeeping
harlansCurseSafekeeping =
  act (2, A) HarlansCurseSafekeeping Cards.harlansCurseSafekeeping Nothing

instance HasAbilities HarlansCurseSafekeeping where
  getAbilities (HarlansCurseSafekeeping a) =
    [ restrictedAbility
          a
          1
          (AssetExists $ assetIs Assets.harlanEarnstone <> AssetWithClues
            (AtLeast $ PerPlayer 1)
          )
        $ Objective
        $ ForcedAbility AnyWindow
    | onSide A a
    ]

instance RunMessage HarlansCurseSafekeeping where
  runMessage msg a@(HarlansCurseSafekeeping attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      relicOfAges <- getSetAsideCard Assets.relicOfAgesADeviceOfSomeSort
      curiositieShoppe <- selectJust $ locationIs Locations.curiositieShoppe
      deckCount <- getActDecksInPlayCount
      acolyteCount <- if deckCount <= 2
        then getPlayerCountValue (ByPlayerCount 1 1 2 2)
        else pure 0
      assetId <- getRandom
      pushAll
        $ CreateAssetAt assetId relicOfAges (AttachedToLocation curiositieShoppe)
        : replicate
            acolyteCount
            (FindEncounterCard
              leadInvestigatorId
              (toTarget attrs)
              [FromEncounterDeck]
              (cardIs Enemies.acolyte)
            )
        <> [AdvanceToAct (actDeckId attrs) Acts.findTheRelic A (toSource attrs)]
      pure a
    FoundEncounterCard _ target card | isTarget attrs target -> do
      curiositieShoppe <- selectJust $ locationIs Locations.curiositieShoppe
      push $ SpawnEnemyAt (EncounterCard card) curiositieShoppe
      pure a
    _ -> HarlansCurseSafekeeping <$> runMessage msg attrs
