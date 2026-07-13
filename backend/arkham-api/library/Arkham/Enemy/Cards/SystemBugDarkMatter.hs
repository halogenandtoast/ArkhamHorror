module Arkham.Enemy.Cards.SystemBugDarkMatter (systemBugDarkMatter) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher

newtype SystemBugDarkMatter = SystemBugDarkMatter EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

systemBugDarkMatter :: EnemyCard SystemBugDarkMatter
systemBugDarkMatter =
  enemy SystemBugDarkMatter Cards.systemBugDarkMatter
    & setSpawnAt (NearestLocationToYou LocationWithoutClues)

instance HasAbilities SystemBugDarkMatter where
  getAbilities (SystemBugDarkMatter a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEntersPlay #after (be a)

instance RunMessage SystemBugDarkMatter where
  runMessage msg e@(SystemBugDarkMatter attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \loc -> placeClues (attrs.ability 1) loc 1
      pure e
    _ -> SystemBugDarkMatter <$> liftRunMessage msg attrs
