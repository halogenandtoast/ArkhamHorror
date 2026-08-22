module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.SecretChamberDarkestDepths (secretChamberDarkestDepths) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype SecretChamberDarkestDepths = SecretChamberDarkestDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretChamberDarkestDepths :: LocationCard SecretChamberDarkestDepths
secretChamberDarkestDepths =
  symbolLabel $ location SecretChamberDarkestDepths Cards.secretChamberDarkestDepths 2 (PerPlayer 2)

instance HasAbilities SecretChamberDarkestDepths where
  getAbilities (SecretChamberDarkestDepths a) = extendRevealed a []

instance RunMessage SecretChamberDarkestDepths where
  runMessage msg (SecretChamberDarkestDepths attrs) = runQueueT $ SecretChamberDarkestDepths <$> liftRunMessage msg attrs
