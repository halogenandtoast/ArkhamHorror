module Arkham.Enemy.Cards.SpawnOfHali (
  spawnOfHali,
  SpawnOfHali (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype SpawnOfHali = SpawnOfHali EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

spawnOfHali :: EnemyCard SpawnOfHali
spawnOfHali =
  enemyWith
    SpawnOfHali
    Cards.spawnOfHali
    (4, Static 4, 2)
    (1, 2)
    (preyL .~ Prey MostHorror)

instance HasAbilities SpawnOfHali where
  getAbilities (SpawnOfHali a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ SkillTestResult
            Timing.After
            You
            (WhileEvadingAnEnemy $ EnemyWithId $ toId a)
            (SuccessResult $ LessThanOrEqualTo $ Static 2)
      ]

instance RunMessage SpawnOfHali where
  runMessage msg e@(SpawnOfHali attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          e <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> SpawnOfHali <$> runMessage msg attrs
