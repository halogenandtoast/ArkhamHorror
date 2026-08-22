module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.HiddenLaboratoryDarkestDepths (hiddenLaboratoryDarkestDepths) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype HiddenLaboratoryDarkestDepths = HiddenLaboratoryDarkestDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenLaboratoryDarkestDepths :: LocationCard HiddenLaboratoryDarkestDepths
hiddenLaboratoryDarkestDepths =
  symbolLabel
    $ location HiddenLaboratoryDarkestDepths Cards.hiddenLaboratoryDarkestDepths 4 (PerPlayer 2)

instance HasAbilities HiddenLaboratoryDarkestDepths where
  getAbilities (HiddenLaboratoryDarkestDepths a) = extendRevealed a []

instance RunMessage HiddenLaboratoryDarkestDepths where
  runMessage msg (HiddenLaboratoryDarkestDepths attrs) = runQueueT $ HiddenLaboratoryDarkestDepths <$> liftRunMessage msg attrs
