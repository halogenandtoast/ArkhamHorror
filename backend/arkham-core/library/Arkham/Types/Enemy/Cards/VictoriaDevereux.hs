module Arkham.Types.Enemy.Cards.VictoriaDevereux
  ( VictoriaDevereux(..)
  , victoriaDevereux
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype VictoriaDevereux = VictoriaDevereux EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victoriaDevereux :: EnemyCard VictoriaDevereux
victoriaDevereux = enemyWith
  VictoriaDevereux
  Cards.victoriaDevereux
  (3, Static 3, 2)
  (1, 0)
  (spawnAtL ?~ LocationWithTitle "Northside")

instance HasAbilities VictoriaDevereux where
  getAbilities (VictoriaDevereux attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 OnSameLocation
        $ ActionAbility (Just Parley) (Costs [ActionCost 1, ResourceCost 5])
    ]

instance EnemyRunner env => RunMessage env VictoriaDevereux where
  runMessage msg e@(VictoriaDevereux attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (AddToVictory $ toTarget attrs)
    _ -> VictoriaDevereux <$> runMessage msg attrs
