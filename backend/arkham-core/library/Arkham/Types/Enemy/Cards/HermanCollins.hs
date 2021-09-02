module Arkham.Types.Enemy.Cards.HermanCollins
  ( HermanCollins(..)
  , hermanCollins
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

newtype HermanCollins = HermanCollins EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hermanCollins :: EnemyCard HermanCollins
hermanCollins = enemyWith
  HermanCollins
  Cards.hermanCollins
  (3, Static 4, 4)
  (1, 1)
  (spawnAtL ?~ LocationWithTitle "Graveyard")

instance HasAbilities HermanCollins where
  getAbilities (HermanCollins attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 OnSameLocation $ ActionAbility
        (Just Parley)
        (Costs [ActionCost 1, HandDiscardCost 4 Nothing mempty mempty])
    ]

instance EnemyRunner env => RunMessage env HermanCollins where
  runMessage msg e@(HermanCollins attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (AddToVictory $ toTarget attrs)
    _ -> HermanCollins <$> runMessage msg attrs
