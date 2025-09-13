module Arkham.Location.Cards.MarshRefineryInTooDeep (marshRefineryInTooDeep) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.InTooDeep.Helpers

newtype MarshRefineryInTooDeep = MarshRefineryInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marshRefineryInTooDeep :: LocationCard MarshRefineryInTooDeep
marshRefineryInTooDeep = locationWith MarshRefineryInTooDeep Cards.marshRefineryInTooDeep 3 (Static 1) connectsToAdjacent

instance HasAbilities MarshRefineryInTooDeep where
  getAbilities (MarshRefineryInTooDeep a) =
    extendRevealed
      a
      [ restricted
          a
          1
          (Here <> youExist InvestigatorWithAnyKey <> CanMoveTo (ConnectedFrom ForMovement (be a)))
          $ FastAbility Free
      , restricted a 2 Here $ actionAbilityWithCost $ DiscardAssetCost (AssetControlledBy You)
      ]

instance RunMessage MarshRefineryInTooDeep where
  runMessage msg l@(MarshRefineryInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      choices <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseTargetM iid choices $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> MarshRefineryInTooDeep <$> liftRunMessage msg attrs
