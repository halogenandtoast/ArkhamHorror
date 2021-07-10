module Arkham.Types.Enemy.Cards.WolfManDrew
  ( WolfManDrew(..)
  , wolfManDrew
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype WolfManDrew = WolfManDrew EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wolfManDrew :: EnemyCard WolfManDrew
wolfManDrew = enemyWith
  WolfManDrew
  Cards.wolfManDrew
  (4, Static 4, 2)
  (2, 0)
  (spawnAtL ?~ LocationWithTitle "Downtown")

instance HasModifiersFor env WolfManDrew

instance ActionRunner env => HasActions env WolfManDrew where
  getActions i window (WolfManDrew attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env WolfManDrew where
  runMessage msg (WolfManDrew attrs) = case msg of
    PerformEnemyAttack _ eid | eid == enemyId attrs ->
      WolfManDrew <$> runMessage msg (attrs & damageL %~ max 0 . subtract 1)
    _ -> WolfManDrew <$> runMessage msg attrs
