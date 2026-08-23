module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.LockedChamberShallowTunnels (
  lockedChamberShallowTunnels,
) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LockedChamberShallowTunnels = LockedChamberShallowTunnels LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedChamberShallowTunnels :: LocationCard LockedChamberShallowTunnels
lockedChamberShallowTunnels =
  symbolLabel
    $ location LockedChamberShallowTunnels Cards.lockedChamberShallowTunnels 3 (PerPlayer 1)

instance HasModifiersFor LockedChamberShallowTunnels where
  getModifiersFor (LockedChamberShallowTunnels a) = unless a.revealed do
    modifySelf
      a
      [ AdditionalCostToEnter
          $ GroupClueCost (PerPlayer 1) (locationIs Cards.cavernEntranceShallowTunnels)
      ]

instance HasAbilities LockedChamberShallowTunnels where
  getAbilities (LockedChamberShallowTunnels a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage LockedChamberShallowTunnels where
  runMessage msg l@(LockedChamberShallowTunnels attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      act <- selectJust AnyAct
      placeClues (attrs.ability 1) act 1
      pure l
    _ -> LockedChamberShallowTunnels <$> liftRunMessage msg attrs
