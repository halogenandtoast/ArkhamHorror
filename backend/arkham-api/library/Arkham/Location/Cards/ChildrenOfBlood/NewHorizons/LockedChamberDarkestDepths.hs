module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.LockedChamberDarkestDepths (
  lockedChamberDarkestDepths,
) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LockedChamberDarkestDepths = LockedChamberDarkestDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedChamberDarkestDepths :: LocationCard LockedChamberDarkestDepths
lockedChamberDarkestDepths =
  symbolLabel
    $ location LockedChamberDarkestDepths Cards.lockedChamberDarkestDepths 3 (PerPlayer 1)

instance HasAbilities LockedChamberDarkestDepths where
  getAbilities (LockedChamberDarkestDepths a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage LockedChamberDarkestDepths where
  runMessage msg l@(LockedChamberDarkestDepths attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      act <- selectJust AnyAct
      placeClues (attrs.ability 1) act 1
      pure l
    _ -> LockedChamberDarkestDepths <$> liftRunMessage msg attrs
