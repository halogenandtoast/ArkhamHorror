module Arkham.Enemy.Cards.VengefulWitch (
  vengefulWitch,
  VengefulWitch (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype VengefulWitch = VengefulWitch EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengefulWitch :: EnemyCard VengefulWitch
vengefulWitch =
  enemyWith
    VengefulWitch
    Cards.vengefulWitch
    (3, Static 3, 3)
    (1, 1)
    ( spawnAtL
        ?~ SpawnLocation
          (LocationMatchAny [LocationWithTitle "The Gallows", LocationWithTitle "Heretics' Graves"])
    )

instance HasAbilities VengefulWitch where
  getAbilities (VengefulWitch a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 (InvestigatorExists $ InvestigatorAt $ locationWithEnemy (toId a)) $
          ForcedAbility $
            EnemyDefeated Timing.When Anyone ByAny $
              EnemyWithId $
                toId a
      ]

instance RunMessage VengefulWitch where
  runMessage msg e@(VengefulWitch attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      damage <- field EnemyHealthDamage (toId attrs)
      horror <- field EnemySanityDamage (toId attrs)
      investigators <- selectList $ InvestigatorAt $ locationWithEnemy (toId attrs)
      pushAll
        [InvestigatorDirectDamage iid (toSource attrs) damage horror | iid <- investigators]
      pure e
    _ -> VengefulWitch <$> runMessage msg attrs
