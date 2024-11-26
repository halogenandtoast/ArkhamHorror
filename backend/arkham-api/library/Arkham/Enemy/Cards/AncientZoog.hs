module Arkham.Enemy.Cards.AncientZoog (ancientZoog, AncientZoog (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Trait (Trait (Zoog))

newtype AncientZoog = AncientZoog EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientZoog :: EnemyCard AncientZoog
ancientZoog =
  enemyWith AncientZoog Cards.ancientZoog (3, Static 3, 3) (1, 1)
    $ spawnAtL
    ?~ SpawnAtFirst
      [ SpawnAt $ LocationWithUnrevealedTitle "Enchanted Woods" <> UnrevealedLocation
      , SpawnAt $ LocationWithUnrevealedTitle "Enchanted Woods"
      ]

instance HasAbilities AncientZoog where
  getAbilities (AncientZoog x) =
    extend1 x
      $ restricted
        x
        1
        (thisIs x (enemy_ #ready) <> exists (withTrait Zoog <> #swarming <> NotEnemy IsSwarm))
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage AncientZoog where
  runMessage msg e@(AncientZoog attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      selectEach (withTrait Zoog <> #swarming <> NotEnemy IsSwarm) \zoog -> do
        push $ PlaceSwarmCards lead zoog 1
      pure e
    _ -> AncientZoog <$> liftRunMessage msg attrs
