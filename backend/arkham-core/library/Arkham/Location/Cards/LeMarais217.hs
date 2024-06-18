module Arkham.Location.Cards.LeMarais217 (leMarais217, LeMarais217 (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype LeMarais217 = LeMarais217 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leMarais217 :: LocationCard LeMarais217
leMarais217 = location LeMarais217 Cards.leMarais217 3 (PerPlayer 1)

instance HasAbilities LeMarais217 where
  getAbilities (LeMarais217 attrs) =
    extendRevealed
      attrs
      [groupLimit PerRound $ restrictedAbility attrs 1 Here (freeReaction $ TurnBegins #when You)]

instance RunMessage LeMarais217 where
  runMessage msg l@(LeMarais217 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (attrs.ability 1) 1
      pure l
    _ -> LeMarais217 <$> runMessage msg attrs
