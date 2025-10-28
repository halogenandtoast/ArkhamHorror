module Arkham.Location.Cards.Yuggoth (yuggoth) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Yuggoth = Yuggoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yuggoth :: LocationCard Yuggoth
yuggoth = location Yuggoth Cards.yuggoth 2 (Static 3)

instance HasAbilities Yuggoth where
  getAbilities (Yuggoth a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> oneOf [CluesOnThis (atLeast 1), CanDrawCards]) actionAbility

instance RunMessage Yuggoth where
  runMessage msg l@(Yuggoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipCluesToDoom attrs 1
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> Yuggoth <$> liftRunMessage msg attrs
