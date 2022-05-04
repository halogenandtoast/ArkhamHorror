module Arkham.Enemy.Cards.BillyCooper
  ( billyCooper
  , BillyCooper(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import qualified Arkham.Timing as Timing
import Arkham.Trait

newtype BillyCooper = BillyCooper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billyCooper :: EnemyCard BillyCooper
billyCooper = enemyWith
  BillyCooper
  Cards.billyCooper
  (5, Static 4, 2)
  (2, 0)
  (spawnAtL ?~ LocationWithTitle "Easttown")

instance HasAbilities BillyCooper where
  getAbilities (BillyCooper attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ EnemyDefeated Timing.After Anyone
      $ EnemyAt (LocationWithId loc)
      <> EnemyWithTrait Monster
    | loc <- maybeToList (enemyLocation attrs)
    ]

instance EnemyRunner env => RunMessage env BillyCooper where
  runMessage msg e@(BillyCooper attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (AddToVictory $ toTarget attrs)
    _ -> BillyCooper <$> runMessage msg attrs
