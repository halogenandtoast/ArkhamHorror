module Arkham.Location.Cards.Northside_c2026 (northside_c2026) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (northside_c2026)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype Northside_c2026 = Northside_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northside_c2026 :: LocationCard Northside_c2026
northside_c2026 = location Northside_c2026 Cards.northside_c2026 3 (PerPlayer 2)

instance HasAbilities Northside_c2026 where
  getAbilities (Northside_c2026 a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> exists (RevealedLocation <> LocationWithTrait Arkham))
      $ actionAbilityWithCost (ResourceCost 5)

instance RunMessage Northside_c2026 where
  runMessage msg l@(Northside_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ RevealedLocation <> LocationWithTrait Arkham
      chooseTargetM iid locations $ discoverAt NotInvestigate iid (attrs.ability 1) 1
      pure l
    _ -> Northside_c2026 <$> liftRunMessage msg attrs
