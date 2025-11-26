module Arkham.Location.Cards.ForgottenOutpost (forgottenOutpost) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForgottenOutpost = ForgottenOutpost LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forgottenOutpost :: LocationCard ForgottenOutpost
forgottenOutpost = location ForgottenOutpost Cards.forgottenOutpost 0 (Static 0)

instance HasAbilities ForgottenOutpost where
  getAbilities (ForgottenOutpost attrs) =
    extendRevealed attrs []

instance RunMessage ForgottenOutpost where
  runMessage msg (ForgottenOutpost attrs) = runQueueT $ case msg of
    _ -> ForgottenOutpost <$> liftRunMessage msg attrs
