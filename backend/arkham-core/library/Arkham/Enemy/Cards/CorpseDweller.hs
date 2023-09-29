module Arkham.Enemy.Cards.CorpseDweller (
  corpseDweller,
  CorpseDweller (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
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
    ( (spawnAtL ?~ SpawnAt (LocationWithEnemy (EnemyWithTrait Humanoid)))
        . (surgeIfUnableToSpawnL .~ True)
    )

instance RunMessage CorpseDweller where
  runMessage msg (CorpseDweller attrs) = case msg of
    EnemySpawn miid lid eid | eid == toId attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      let iid = fromMaybe leadInvestigatorId miid
      humanoids <-
        selectList
          $ EnemyWithTrait Humanoid
          <> EnemyAt
            (LocationWithId lid)
      push
        $ chooseOrRunOne
          iid
          [ targetLabel humanoid [Discard (toSource attrs) (EnemyTarget humanoid)]
          | humanoid <- humanoids
          ]
      CorpseDweller <$> runMessage msg attrs
    _ -> CorpseDweller <$> runMessage msg attrs
