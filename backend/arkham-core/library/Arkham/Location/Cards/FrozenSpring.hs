module Arkham.Location.Cards.FrozenSpring
  ( frozenSpring
  , FrozenSpring(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( frozenSpring )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype FrozenSpring = FrozenSpring LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenSpring :: LocationCard FrozenSpring
frozenSpring = location FrozenSpring Cards.frozenSpring 3 (PerPlayer 1)

instance HasAbilities FrozenSpring where
  getAbilities (FrozenSpring attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage FrozenSpring where
  runMessage msg l@(FrozenSpring attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      l <$ pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
    _ -> FrozenSpring <$> runMessage msg attrs
