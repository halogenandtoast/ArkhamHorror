module Arkham.Location.Cards.Southside_c2026 (southside_c2026) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (southside_c2026)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Southside_c2026 = Southside_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southside_c2026 :: LocationCard Southside_c2026
southside_c2026 = location Southside_c2026 Cards.southside_c2026 2 (PerPlayer 2)

instance HasAbilities Southside_c2026 where
  getAbilities (Southside_c2026 a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (investigatorAt a <> can.draw.cards)) doubleActionAbility

instance RunMessage Southside_c2026 where
  runMessage msg l@(Southside_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      doStep 3 msg
      pure l
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      investigators <- select (investigatorAt attrs <> can.draw.cards)
      chooseTargetM iid investigators \iid' -> drawCards iid' (attrs.ability 1) 1
      doStep (n - 1) msg'
      pure l
    _ -> Southside_c2026 <$> liftRunMessage msg attrs
