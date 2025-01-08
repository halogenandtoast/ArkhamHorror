module Arkham.Location.Cards.Cellar (cellar) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (cellar)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Cellar = Cellar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cellar :: LocationCard Cellar
cellar = location Cellar Cards.cellar 4 (PerPlayer 2)

instance HasAbilities Cellar where
  getAbilities (Cellar a) =
    extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage Cellar where
  runMessage msg a@(Cellar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure a
    _ -> Cellar <$> liftRunMessage msg attrs
