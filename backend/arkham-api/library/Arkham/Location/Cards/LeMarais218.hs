module Arkham.Location.Cards.LeMarais218 (leMarais218, LeMarais218 (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype LeMarais218 = LeMarais218 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leMarais218 :: LocationCard LeMarais218
leMarais218 = location LeMarais218 Cards.leMarais218 1 (PerPlayer 1)

instance HasAbilities LeMarais218 where
  getAbilities (LeMarais218 a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ Enters #after You (be a)

instance RunMessage LeMarais218 where
  runMessage msg l@(LeMarais218 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) iid CannotMove
      pure l
    _ -> LeMarais218 <$> liftRunMessage msg attrs
