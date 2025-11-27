module Arkham.Location.Cards.ForgottenOutpost (forgottenOutpost) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForgottenOutpost = ForgottenOutpost LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forgottenOutpost :: LocationCard ForgottenOutpost
forgottenOutpost = location ForgottenOutpost Cards.forgottenOutpost 4 (PerPlayer 1)

instance HasAbilities ForgottenOutpost where
  getAbilities (ForgottenOutpost a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> youExist can.draw.cards) actionAbility

instance RunMessage ForgottenOutpost where
  runMessage msg l@(ForgottenOutpost attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 3
      pure l
    _ -> ForgottenOutpost <$> liftRunMessage msg attrs
