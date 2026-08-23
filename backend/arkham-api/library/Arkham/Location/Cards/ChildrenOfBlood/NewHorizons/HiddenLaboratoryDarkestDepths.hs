module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.HiddenLaboratoryDarkestDepths (
  hiddenLaboratoryDarkestDepths,
) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HiddenLaboratoryDarkestDepths = HiddenLaboratoryDarkestDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenLaboratoryDarkestDepths :: LocationCard HiddenLaboratoryDarkestDepths
hiddenLaboratoryDarkestDepths =
  symbolLabel
    $ location HiddenLaboratoryDarkestDepths Cards.hiddenLaboratoryDarkestDepths 3 (PerPlayer 1)

instance HasAbilities HiddenLaboratoryDarkestDepths where
  getAbilities (HiddenLaboratoryDarkestDepths a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (AddTokenCost 1 #blood)

instance RunMessage HiddenLaboratoryDarkestDepths where
  runMessage msg l@(HiddenLaboratoryDarkestDepths attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      act <- selectJust AnyAct
      placeClues (attrs.ability 1) act 1
      pure l
    _ -> HiddenLaboratoryDarkestDepths <$> liftRunMessage msg attrs
