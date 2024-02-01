module Arkham.Enemy.Cards.AncientZoog (
  ancientZoog,
  AncientZoog (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (Zoog))

newtype AncientZoog = AncientZoog EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ancientZoog :: EnemyCard AncientZoog
ancientZoog =
  enemyWith
    AncientZoog
    Cards.ancientZoog
    (3, Static 3, 3)
    (1, 1)
    ( spawnAtL
        ?~ SpawnAtFirst
          [ SpawnAt $ LocationWithUnrevealedTitle "Enchanted Woods" <> UnrevealedLocation
          , SpawnAt $ LocationWithUnrevealedTitle "Enchanted Woods"
          ]
    )

instance HasAbilities AncientZoog where
  getAbilities (AncientZoog x) =
    withBaseAbilities x
      $ [ restrictedAbility
            x
            1
            ( exists (ReadyEnemy <> EnemyWithId (toId x))
                <> exists (EnemyWithTrait Zoog <> SwarmingEnemy <> NotEnemy IsSwarm)
            )
            $ ForcedAbility
            $ PhaseBegins #when #enemy
        ]

instance RunMessage AncientZoog where
  runMessage msg e@(AncientZoog attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      zoogs <- selectList $ EnemyWithTrait Zoog <> SwarmingEnemy <> NotEnemy IsSwarm
      lead <- getLead
      for_ zoogs $ \zoog ->
        push $ PlaceSwarmCards lead zoog 1
      pure e
    _ -> AncientZoog <$> runMessage msg attrs
