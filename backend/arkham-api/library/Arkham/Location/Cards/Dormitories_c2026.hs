module Arkham.Location.Cards.Dormitories_c2026 (dormitories_c2026) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (dormitories_c2026)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Dormitories_c2026 = Dormitories_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormitories_c2026 :: LocationCard Dormitories_c2026
dormitories_c2026 = location Dormitories_c2026 Cards.dormitories_c2026 3 (PerPlayer 1)

instance HasAbilities Dormitories_c2026 where
  getAbilities (Dormitories_c2026 attrs) =
    extendRevealed1 attrs
      $ playerLimit PerGame
      $ withCriteria
        (mkAbility attrs 1 actionAbility)
        (Here <> any_ [HealableInvestigator (toSource attrs) kind You | kind <- [#horror, #damage]])

instance RunMessage Dormitories_c2026 where
  runMessage msg l@(Dormitories_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamage iid (attrs.ability 1) 1
      healHorror iid (attrs.ability 1) 1
      pure l
    _ -> Dormitories_c2026 <$> liftRunMessage msg attrs
