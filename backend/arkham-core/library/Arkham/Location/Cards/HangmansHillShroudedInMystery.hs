module Arkham.Location.Cards.HangmansHillShroudedInMystery (
  hangmansHillShroudedInMystery,
  HangmansHillShroudedInMystery (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype HangmansHillShroudedInMystery = HangmansHillShroudedInMystery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hangmansHillShroudedInMystery :: LocationCard HangmansHillShroudedInMystery
hangmansHillShroudedInMystery = location HangmansHillShroudedInMystery Cards.hangmansHillShroudedInMystery 4 (PerPlayer 1)

instance HasAbilities HangmansHillShroudedInMystery where
  getAbilities (HangmansHillShroudedInMystery attrs) =
    withRevealedAbilities
      attrs
      [fastAbility attrs 1 (DamageCost (toAbilitySource attrs 1) YouTarget 1) Here]

instance RunMessage HangmansHillShroudedInMystery where
  runMessage msg l@(HangmansHillShroudedInMystery attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      act <- selectJust AnyAct
      pushAll [RemoveBreaches (toTarget attrs) 1, PlaceBreaches (toTarget act) 1]
      pure l
    _ -> HangmansHillShroudedInMystery <$> runMessage msg attrs
