module Arkham.Location.Cards.GilmanHouse (gilmanHouse, GilmanHouse (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.Healing
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GilmanHouse = GilmanHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gilmanHouse :: LocationCard GilmanHouse
gilmanHouse = location GilmanHouse Cards.gilmanHouse 2 (PerPlayer 1)

instance HasAbilities GilmanHouse where
  getAbilities (GilmanHouse attrs) =
    extendRevealed1 attrs
      $ playerLimit PerGame
      $ restrictedAbility attrs 1 (Here <> can.heal.any (attrs.ability 1) You) actionAbility

instance RunMessage GilmanHouse where
  runMessage msg l@(GilmanHouse attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      twice $ doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      chooseHealDamageOrHorror (attrs.ability 1) iid
      pure l
    _ -> GilmanHouse <$> liftRunMessage msg attrs
