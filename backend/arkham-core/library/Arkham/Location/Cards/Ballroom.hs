module Arkham.Location.Cards.Ballroom (ballroom, Ballroom (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Ballroom = Ballroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ballroom :: LocationCard Ballroom
ballroom = location Ballroom Cards.ballroom 4 (Static 0)

instance HasAbilities Ballroom where
  getAbilities (Ballroom attrs) =
    withRevealedAbilities
      attrs
      [ groupLimit PerPhase
          $ restrictedAbility attrs 1 Here
          $ freeReaction
          $ PerformAction #after You #parley
      ]

instance RunMessage Ballroom where
  runMessage msg l@(Ballroom attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ TakeResources iid 2 (attrs.ability 1) False
      pure l
    _ -> Ballroom <$> runMessage msg attrs
