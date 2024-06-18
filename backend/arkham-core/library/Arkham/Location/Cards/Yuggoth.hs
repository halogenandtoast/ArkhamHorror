module Arkham.Location.Cards.Yuggoth (yuggoth, Yuggoth (..)) where

import Arkham.Ability
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Yuggoth = Yuggoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yuggoth :: LocationCard Yuggoth
yuggoth = location Yuggoth Cards.yuggoth 2 (Static 3)

instance HasAbilities Yuggoth where
  getAbilities (Yuggoth a) =
    withBaseAbilities
      a
      [restrictedAbility a 1 (Here <> oneOf [CluesOnThis (atLeast 1), CanDrawCards]) actionAbility]

instance RunMessage Yuggoth where
  runMessage msg l@(Yuggoth attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let drawCard = newCardDraw attrs iid 1
      pushAll [FlipClues (toTarget attrs) 1, DrawCards iid drawCard]
      pure l
    _ -> Yuggoth <$> runMessage msg attrs
