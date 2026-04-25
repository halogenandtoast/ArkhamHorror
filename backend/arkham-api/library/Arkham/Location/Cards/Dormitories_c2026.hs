module Arkham.Location.Cards.Dormitories_c2026 (dormitories_c2026) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (dormitories_c2026)
import Arkham.Location.Import.Lifted

newtype Dormitories_c2026 = Dormitories_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormitories_c2026 :: LocationCard Dormitories_c2026
dormitories_c2026 = location Dormitories_c2026 Cards.dormitories_c2026 3 (PerPlayer 1)

instance HasAbilities Dormitories_c2026 where
  getAbilities (Dormitories_c2026 a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 Here actionAbility

instance RunMessage Dormitories_c2026 where
  runMessage msg l@(Dormitories_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamage iid (attrs.ability 1) 1
      healHorror iid (attrs.ability 1) 1
      pure l
    _ -> Dormitories_c2026 <$> liftRunMessage msg attrs
