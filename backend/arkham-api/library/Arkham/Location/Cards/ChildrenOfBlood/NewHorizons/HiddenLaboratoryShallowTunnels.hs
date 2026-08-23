module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.HiddenLaboratoryShallowTunnels (
  hiddenLaboratoryShallowTunnels,
) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HiddenLaboratoryShallowTunnels = HiddenLaboratoryShallowTunnels LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenLaboratoryShallowTunnels :: LocationCard HiddenLaboratoryShallowTunnels
hiddenLaboratoryShallowTunnels =
  symbolLabel
    $ location HiddenLaboratoryShallowTunnels Cards.hiddenLaboratoryShallowTunnels 3 (PerPlayer 1)

instance HasModifiersFor HiddenLaboratoryShallowTunnels where
  getModifiersFor (HiddenLaboratoryShallowTunnels a) = unless a.revealed do
    modifySelf
      a
      [ AdditionalCostToEnter
          $ GroupClueCost (PerPlayer 1) (locationIs Cards.cavernEntranceShallowTunnels)
      ]

instance HasAbilities HiddenLaboratoryShallowTunnels where
  getAbilities (HiddenLaboratoryShallowTunnels a) =
    extendRevealed1 a $ groupLimit PerGame $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage HiddenLaboratoryShallowTunnels where
  runMessage msg l@(HiddenLaboratoryShallowTunnels attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      act <- selectJust AnyAct
      placeClues (attrs.ability 1) act 1
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 4)
      pure l
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      addChaosToken #blood
      pure l
    _ -> HiddenLaboratoryShallowTunnels <$> liftRunMessage msg attrs
