{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.Southside_Arkham (southside_Arkham) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (southside_Arkham)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Southside_Arkham = Southside_Arkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southside_Arkham :: LocationCard Southside_Arkham
southside_Arkham = location Southside_Arkham Cards.southside_Arkham 2 (PerPlayer 2)

instance HasAbilities Southside_Arkham where
  getAbilities (Southside_Arkham a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (investigatorAt a <> can.draw.cards)) doubleActionAbility

instance RunMessage Southside_Arkham where
  runMessage msg l@(Southside_Arkham attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      doStep 3 msg
      pure l
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      investigators <- select (investigatorAt attrs <> can.draw.cards)
      chooseTargetM iid investigators \iid' -> drawCards iid' (attrs.ability 1) 1
      doStep (n - 1) msg'
      pure l
    _ -> Southside_Arkham <$> liftRunMessage msg attrs
