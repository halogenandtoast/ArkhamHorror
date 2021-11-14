module Arkham.Types.Enemy.Cards.SpawnOfHali
  ( spawnOfHali
  , SpawnOfHali(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Timing qualified as Timing

newtype SpawnOfHali = SpawnOfHali EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spawnOfHali :: EnemyCard SpawnOfHali
spawnOfHali = enemyWith
  SpawnOfHali
  Cards.spawnOfHali
  (4, Static 4, 2)
  (1, 2)
  (preyL .~ MostHorror)

instance HasAbilities SpawnOfHali where
  getAbilities (SpawnOfHali a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ SkillTestResult
        Timing.After
        You
        (WhileEvadingAnEnemy $ EnemyWithId $ toId a)
        (SuccessResult $ LessThanOrEqualTo $ Static 2)
    ]

instance EnemyRunner env => RunMessage env SpawnOfHali where
  runMessage msg e@(SpawnOfHali attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> SpawnOfHali <$> runMessage msg attrs
