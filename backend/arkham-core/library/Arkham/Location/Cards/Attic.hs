module Arkham.Location.Cards.Attic where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (attic)
import Arkham.Location.Runner
import Arkham.Matcher

newtype Attic = Attic LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

attic :: LocationCard Attic
attic = location Attic Cards.attic 1 (PerPlayer 2)

instance HasAbilities Attic where
  getAbilities (Attic a) = withRevealedAbilities a [forcedAbility a 1 $ Enters #after You $ be a]

instance RunMessage Attic where
  runMessage msg a@(Attic attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ assignHorror iid (toAbilitySource attrs 1) 1
      pure a
    _ -> Attic <$> runMessage msg attrs
