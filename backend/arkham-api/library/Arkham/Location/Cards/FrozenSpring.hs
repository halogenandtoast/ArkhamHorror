module Arkham.Location.Cards.FrozenSpring (frozenSpring) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (frozenSpring)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FrozenSpring = FrozenSpring LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenSpring :: LocationCard FrozenSpring
frozenSpring = location FrozenSpring Cards.frozenSpring 3 (PerPlayer 1)

instance HasAbilities FrozenSpring where
  getAbilities (FrozenSpring a) = extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage FrozenSpring where
  runMessage msg l@(FrozenSpring attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      setActions iid (attrs.ability 1) 0
      endYourTurn iid
      pure l
    _ -> FrozenSpring <$> liftRunMessage msg attrs
