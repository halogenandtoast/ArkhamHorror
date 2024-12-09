module Arkham.Enemy.Cards.SlitheringDhole (slitheringDhole, SlitheringDhole (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Treacheries

newtype SlitheringDhole = SlitheringDhole EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slitheringDhole :: EnemyCard SlitheringDhole
slitheringDhole =
  enemyWith
    SlitheringDhole
    Cards.slitheringDhole
    (3, Static 5, 3)
    (1, 1)
    $ ( spawnAtL
          ?~ SpawnAt (NearestLocationToYou $ LocationWithTreachery $ treacheryIs Treacheries.dholeTunnel)
      )
    . (surgeIfUnableToSpawnL .~ True)
    . (unableToSpawnL .~ ShuffleBackInIfUnableToSpawn)

instance HasModifiersFor SlitheringDhole where
  getModifiersFor (SlitheringDhole a) = do
    modifySelectMap a Anywhere \lid ->
      [ ConnectedToWhen
          ( LocationWithTreachery (treacheryIs Treacheries.dholeTunnel)
              <> LocationWhenCriteria (exists $ EnemyWithId a.id <> MovingEnemy)
          )
          (LocationWithTreachery (treacheryIs Treacheries.dholeTunnel) <> not_ (LocationWithId lid))
      ]

instance RunMessage SlitheringDhole where
  runMessage msg (SlitheringDhole attrs) = SlitheringDhole <$> runMessage msg attrs
