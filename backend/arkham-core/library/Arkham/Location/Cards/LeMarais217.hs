module Arkham.Location.Cards.LeMarais217
  ( leMarais217
  , LeMarais217(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype LeMarais217 = LeMarais217 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leMarais217 :: LocationCard LeMarais217
leMarais217 = location LeMarais217 Cards.leMarais217 3 (PerPlayer 1)

instance HasAbilities LeMarais217 where
  getAbilities (LeMarais217 attrs) = withBaseAbilities
    attrs
    [ limitedAbility (GroupLimit PerRound 1) $ restrictedAbility
        attrs
        1
        Here
        (ReactionAbility (TurnBegins Timing.When You) Free)
    | locationRevealed attrs
    ]

instance RunMessage LeMarais217 where
  runMessage msg l@(LeMarais217 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 1
      push drawing
      pure l
    _ -> LeMarais217 <$> runMessage msg attrs
