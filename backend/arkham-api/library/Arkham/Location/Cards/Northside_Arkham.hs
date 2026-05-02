{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.Northside_Arkham (northside_Arkham) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (northside_Arkham)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype Northside_Arkham = Northside_Arkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northside_Arkham :: LocationCard Northside_Arkham
northside_Arkham = location Northside_Arkham Cards.northside_Arkham 3 (PerPlayer 2)

instance HasAbilities Northside_Arkham where
  getAbilities (Northside_Arkham a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> exists (RevealedLocation <> LocationWithTrait Arkham))
      $ actionAbilityWithCost (ResourceCost 5)

instance RunMessage Northside_Arkham where
  runMessage msg l@(Northside_Arkham attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ RevealedLocation <> LocationWithTrait Arkham
      chooseTargetM iid locations $ discoverAt NotInvestigate iid (attrs.ability 1) 1
      pure l
    _ -> Northside_Arkham <$> liftRunMessage msg attrs
