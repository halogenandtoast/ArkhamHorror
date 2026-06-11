module Arkham.Location.Cards.StreetsOfMontreal (streetsOfMontreal) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (AncientOne, Montreal))

newtype StreetsOfMontreal = StreetsOfMontreal LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetsOfMontreal :: LocationCard StreetsOfMontreal
streetsOfMontreal = symbolLabel $ location StreetsOfMontreal Cards.streetsOfMontreal 4 (PerPlayer 1)

instance HasAbilities StreetsOfMontreal where
  getAbilities (StreetsOfMontreal a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (exists $ EnemyAt (LocationWithTrait Montreal) <> not_ (EnemyWithTrait AncientOne))
      $ doubleActionAbility

instance RunMessage StreetsOfMontreal where
  runMessage msg l@(StreetsOfMontreal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (LocationWithTrait Montreal) <> not_ (EnemyWithTrait AncientOne)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 3
      pure l
    _ -> StreetsOfMontreal <$> liftRunMessage msg attrs
