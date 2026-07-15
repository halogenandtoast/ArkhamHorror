module Arkham.Homebrew.DarkMatter.Enemies.SystemBug (systemBug) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher

newtype SystemBug = SystemBug EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

systemBug :: EnemyCard SystemBug
systemBug =
  enemy SystemBug Cards.systemBug
    & setSpawnAt (NearestLocationToYou LocationWithoutClues)

instance HasAbilities SystemBug where
  getAbilities (SystemBug a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEntersPlay #after (be a)

instance RunMessage SystemBug where
  runMessage msg e@(SystemBug attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \loc -> placeClues (attrs.ability 1) loc 1
      pure e
    _ -> SystemBug <$> liftRunMessage msg attrs
