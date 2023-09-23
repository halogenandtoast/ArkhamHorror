module Arkham.Location.Cards.Cellar where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (cellar)
import Arkham.Location.Runner
import Arkham.Matcher

newtype Cellar = Cellar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cellar :: LocationCard Cellar
cellar = location Cellar Cards.cellar 4 (PerPlayer 2)

instance HasAbilities Cellar where
  getAbilities (Cellar a) =
    withBaseAbilities a [forcedAbility a 1 $ Enters #after You $ be a]

instance RunMessage Cellar where
  runMessage msg a@(Cellar attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ assignDamage iid (toAbilitySource attrs 1) 1
      pure a
    _ -> Cellar <$> runMessage msg attrs
