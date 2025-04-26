module Arkham.Enemy.Cards.CorpseDweller (corpseDweller) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype CorpseDweller = CorpseDweller EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

corpseDweller :: EnemyCard CorpseDweller
corpseDweller =
  enemyWith
    CorpseDweller
    Cards.corpseDweller
    (3, Static 5, 4)
    (2, 1)
    $ (spawnAtL ?~ SpawnAt (LocationWithEnemy (EnemyWithTrait Humanoid)))
    . (surgeIfUnableToSpawnL .~ True)

instance RunMessage CorpseDweller where
  runMessage msg (CorpseDweller attrs) = case msg of
    EnemySpawn details | details.enemy == attrs.id -> do
      for_ details.location \lid -> do
        let miid = details.investigator
        leadInvestigatorId <- getLead
        let iid = fromMaybe leadInvestigatorId miid
        humanoids <- select $ EnemyWithTrait Humanoid <> enemyAt lid
        player <- getPlayer iid
        push
          $ chooseOrRunOne player
          $ targetLabels humanoids
          $ only
          . toDiscard attrs
      CorpseDweller <$> runMessage msg attrs
    _ -> CorpseDweller <$> runMessage msg attrs
