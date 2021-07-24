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
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Window

newtype VictoriaDevereux = VictoriaDevereux EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victoriaDevereux :: EnemyCard VictoriaDevereux
victoriaDevereux = enemyWith
  VictoriaDevereux
  Cards.victoriaDevereux
  (3, Static 3, 2)
  (1, 0)
  (spawnAtL ?~ LocationWithTitle "Northside")

instance HasModifiersFor env VictoriaDevereux

instance ActionRunner env => HasActions env VictoriaDevereux where
  getActions iid NonFast (VictoriaDevereux attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ UseAbility
            iid
            (mkAbility
              (EnemySource enemyId)
              1
              (ActionAbility
                (Just Parley)
                (Costs [ActionCost 1, ResourceCost 5])
              )
            )
        | locationId == enemyLocation
        ]
  getActions _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env VictoriaDevereux where
  runMessage msg e@(VictoriaDevereux attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (AddToVictory $ toTarget attrs)
    _ -> VictoriaDevereux <$> runMessage msg attrs
