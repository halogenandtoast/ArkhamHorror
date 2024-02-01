module Arkham.Location.Cards.LeMarais217 (
  leMarais217,
  LeMarais217 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype LeMarais217 = LeMarais217 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

leMarais217 :: LocationCard LeMarais217
leMarais217 = location LeMarais217 Cards.leMarais217 3 (PerPlayer 1)

instance HasAbilities LeMarais217 where
  getAbilities (LeMarais217 attrs) =
    withRevealedAbilities attrs
      $ [ limitedAbility (GroupLimit PerRound 1)
            $ restrictedAbility attrs 1 Here (ReactionAbility (TurnBegins Timing.When You) Free)
        ]

instance RunMessage LeMarais217 where
  runMessage msg l@(LeMarais217 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure l
    _ -> LeMarais217 <$> runMessage msg attrs
