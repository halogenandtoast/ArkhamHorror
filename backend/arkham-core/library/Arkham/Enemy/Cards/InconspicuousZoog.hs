module Arkham.Enemy.Cards.InconspicuousZoog (
  inconspicuousZoog,
  InconspicuousZoog (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Placement

newtype InconspicuousZoog = InconspicuousZoog EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inconspicuousZoog :: EnemyCard InconspicuousZoog
inconspicuousZoog =
  enemyWith
    InconspicuousZoog
    Cards.inconspicuousZoog
    (2, Static 1, 2)
    (1, 1)
    (spawnAtL ?~ SpawnAt ConnectedLocation)

instance HasAbilities InconspicuousZoog where
  getAbilities (InconspicuousZoog x) =
    withBaseAbilities
      x
      [ restrictedAbility x 1 isSwarmRestriction
          $ ForcedAbility
          $ EnemyDefeated #when You ByAny
          $ EnemyWithId
          $ toId x
      ]
   where
    isSwarmRestriction = case enemyPlacement x of
      AsSwarm _ _ -> NoRestriction
      _ -> Never

instance RunMessage InconspicuousZoog where
  runMessage msg e@(InconspicuousZoog attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      popMessageMatching_ \case
        ExcessDamage eid' _ | toId attrs == eid' -> True
        _ -> False
      case enemyPlacement attrs of
        AsSwarm host _ -> do
          connectingLocations <- select ConnectedLocation
          player <- getPlayer iid
          pushIfAny connectingLocations
            $ chooseOrRunOne
              player
              [targetLabel location [EnemyMove host location] | location <- connectingLocations]
        _ -> error "should not trigger"
      pure e
    _ -> InconspicuousZoog <$> runMessage msg attrs
