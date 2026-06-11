module Arkham.Location.Cards.StreetsOfProvidence (streetsOfProvidence) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (AncientOne, Providence))

newtype StreetsOfProvidence = StreetsOfProvidence LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetsOfProvidence :: LocationCard StreetsOfProvidence
streetsOfProvidence = symbolLabel $ location StreetsOfProvidence Cards.streetsOfProvidence 4 (PerPlayer 1)

instance HasAbilities StreetsOfProvidence where
  getAbilities (StreetsOfProvidence a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (exists $ EnemyAt (LocationWithTrait Providence) <> not_ (EnemyWithTrait AncientOne))
      $ doubleActionAbility

instance RunMessage StreetsOfProvidence where
  runMessage msg l@(StreetsOfProvidence attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (LocationWithTrait Providence) <> not_ (EnemyWithTrait AncientOne)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 3
      pure l
    _ -> StreetsOfProvidence <$> liftRunMessage msg attrs
