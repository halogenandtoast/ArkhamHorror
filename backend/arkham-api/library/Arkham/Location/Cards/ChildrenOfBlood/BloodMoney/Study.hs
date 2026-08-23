module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.Study (study) where

import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted

newtype Study = Study LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

study :: LocationCard Study
study = symbolLabel $ location Study Cards.study 3 (PerPlayer 1)

instance HasAbilities Study where
  getAbilities (Study a) = extendRevealed a []

instance RunMessage Study where
  runMessage msg (Study attrs) = runQueueT $ Study <$> liftRunMessage msg attrs
