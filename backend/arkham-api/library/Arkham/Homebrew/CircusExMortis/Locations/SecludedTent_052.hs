module Arkham.Homebrew.CircusExMortis.Locations.SecludedTent_052 (secludedTent_052) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype SecludedTent_052 = SecludedTent_052 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secludedTent_052 :: LocationCard SecludedTent_052
secludedTent_052 = location SecludedTent_052 Cards.secludedTent_052 3 (Static 2)

instance HasAbilities SecludedTent_052 where
  getAbilities (SecludedTent_052 a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here
      $ actionAbilityWithCost (PlaceClueOnLocationCost (Static 1))

instance RunMessage SecludedTent_052 where
  runMessage msg l@(SecludedTent_052 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) iid
        $ CannotBeHuntedBy (NonEliteEnemy <> EnemyAt (not_ $ locationWithInvestigator iid))
      pure l
    _ -> SecludedTent_052 <$> liftRunMessage msg attrs
