module Arkham.Enemy.Cards.BillyCooper (
  billyCooper,
  BillyCooper (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype BillyCooper = BillyCooper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billyCooper :: EnemyCard BillyCooper
billyCooper =
  enemyWith
    BillyCooper
    Cards.billyCooper
    (5, Static 4, 2)
    (2, 0)
    (spawnAtL ?~ SpawnLocation (LocationWithTitle "Easttown"))

instance HasAbilities BillyCooper where
  getAbilities (BillyCooper attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ EnemyDefeated Timing.After Anyone ByAny
          $ EnemyAt LocationOfThis
          <> EnemyWithTrait Monster
      ]

instance RunMessage BillyCooper where
  runMessage msg e@(BillyCooper attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          e <$ push (AddToVictory $ toTarget attrs)
    _ -> BillyCooper <$> runMessage msg attrs
