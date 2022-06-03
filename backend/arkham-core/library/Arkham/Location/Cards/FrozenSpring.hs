module Arkham.Location.Cards.FrozenSpring
  ( frozenSpring
  , FrozenSpring(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (frozenSpring)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message hiding (RevealLocation)
import Arkham.Timing qualified as Timing

newtype FrozenSpring = FrozenSpring LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenSpring :: LocationCard FrozenSpring
frozenSpring = locationWithRevealedSideConnections
  FrozenSpring
  Cards.frozenSpring
  3
  (PerPlayer 1)
  NoSymbol
  []
  Plus
  [Triangle, Hourglass]

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

instance LocationRunner env => RunMessage FrozenSpring where
  runMessage msg l@(FrozenSpring attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
    _ -> FrozenSpring <$> runMessage msg attrs
