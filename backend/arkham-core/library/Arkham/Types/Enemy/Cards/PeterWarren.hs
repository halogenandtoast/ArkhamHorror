module Arkham.Types.Enemy.Cards.PeterWarren
  ( PeterWarren(..)
  , peterWarren
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype PeterWarren = PeterWarren EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterWarren :: EnemyCard PeterWarren
peterWarren = enemyWith
  PeterWarren
  Cards.peterWarren
  (2, Static 3, 3)
  (1, 0)
  (spawnAtL ?~ LocationWithTitle "Miskatonic University")

instance ActionRunner env => HasAbilities env PeterWarren where
  getAbilities iid window@(Window Timing.When NonFast) (PeterWarren attrs@EnemyAttrs {..})
    = withBaseActions iid window attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ mkAbility attrs 1
            $ ActionAbility (Just Parley) (Costs [ActionCost 1, ClueCost 2])
        | locationId == enemyLocation
        ]
  getAbilities _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env PeterWarren where
  runMessage msg e@(PeterWarren attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ (EnemySource eid) _ 1 _ | eid == enemyId ->
      e <$ push (AddToVictory $ toTarget attrs)
    _ -> PeterWarren <$> runMessage msg attrs
