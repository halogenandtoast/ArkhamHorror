{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.BrethrenOfAsh.Arkham.Northside (northside) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.CardDefs.BrethrenOfAsh.Arkham qualified as Cards (northside)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype Northside = Northside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northside :: LocationCard Northside
northside = location Northside Cards.northside 3 (PerPlayer 2)

instance HasAbilities Northside where
  getAbilities (Northside a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> exists (RevealedLocation <> LocationWithTrait Arkham))
      $ actionAbilityWithCost (ResourceCost 5)

instance RunMessage Northside where
  runMessage msg l@(Northside attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ RevealedLocation <> LocationWithTrait Arkham
      chooseTargetM iid locations $ discoverAt NotInvestigate iid (attrs.ability 1) 1
      pure l
    _ -> Northside <$> liftRunMessage msg attrs
