module Arkham.Location.Cards.StreetsOfNewYorkCity (streetsOfNewYorkCity) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (AncientOne, NewYorkCity))

newtype StreetsOfNewYorkCity = StreetsOfNewYorkCity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetsOfNewYorkCity :: LocationCard StreetsOfNewYorkCity
streetsOfNewYorkCity = symbolLabel $ location StreetsOfNewYorkCity Cards.streetsOfNewYorkCity 4 (PerPlayer 1)

instance HasAbilities StreetsOfNewYorkCity where
  getAbilities (StreetsOfNewYorkCity a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (exists $ EnemyAt (LocationWithTrait NewYorkCity) <> not_ (EnemyWithTrait AncientOne))
      $ doubleActionAbility

instance RunMessage StreetsOfNewYorkCity where
  runMessage msg l@(StreetsOfNewYorkCity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (LocationWithTrait NewYorkCity) <> not_ (EnemyWithTrait AncientOne)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 3
      pure l
    _ -> StreetsOfNewYorkCity <$> liftRunMessage msg attrs
