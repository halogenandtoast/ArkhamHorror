module Arkham.Location.Cards.Morgue (
  morgue,
  Morgue (..),
)
where

import Arkham.Prelude

import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers

newtype Morgue = Morgue LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

morgue :: LocationCard Morgue
morgue = location Morgue Cards.morgue 5 (PerPlayer 1)

instance HasAbilities Morgue where
  getAbilities (Morgue attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here actionAbility
      , mkAbility attrs 2 $ forced $ DiscoveringLastClue #after Anyone $ LocationWithId $ toId attrs
      ]

instance RunMessage Morgue where
  runMessage msg l@(Morgue attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (attrs.ability 1) iid #willpower 3
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toMessage $ discover iid (toId attrs) (attrs.ability 1) 1
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      pushM makeInfestationTest
      pure l
    _ -> Morgue <$> runMessage msg attrs
