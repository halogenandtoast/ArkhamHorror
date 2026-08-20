module Arkham.Enemy.Cards.TheDreamEaters.PointOfNoReturn.SlitheringDhole (slitheringDhole) where

import Arkham.Classes
import Arkham.Enemy.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Treacheries

newtype SlitheringDhole = SlitheringDhole EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slitheringDhole :: EnemyCard SlitheringDhole
slitheringDhole =
  enemyWith
    SlitheringDhole
    Cards.slitheringDhole
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
