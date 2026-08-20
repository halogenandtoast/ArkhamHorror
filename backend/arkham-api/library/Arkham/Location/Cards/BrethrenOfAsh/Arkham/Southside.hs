{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.BrethrenOfAsh.Arkham.Southside (southside) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.CardDefs.BrethrenOfAsh.Arkham qualified as Cards (southside)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Southside = Southside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southside :: LocationCard Southside
southside = location Southside Cards.southside 2 (PerPlayer 2)

instance HasAbilities Southside where
  getAbilities (Southside a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (investigatorAt a <> can.draw.cards)) doubleActionAbility

instance RunMessage Southside where
  runMessage msg l@(Southside attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      doStep 3 msg
      pure l
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      investigators <- select (investigatorAt attrs <> can.draw.cards)
      chooseTargetM iid investigators \iid' -> drawCards iid' (attrs.ability 1) 1
      doStep (n - 1) msg'
      pure l
    _ -> Southside <$> liftRunMessage msg attrs
