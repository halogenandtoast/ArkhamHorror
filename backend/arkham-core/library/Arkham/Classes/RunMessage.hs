module Arkham.Classes.RunMessage where

import Arkham.Prelude hiding ( to )

import Arkham.Asset.Attrs ( AssetAttrs )
import Arkham.Classes.GameLogger
import Arkham.Classes.HasHistory
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Enemy.Attrs ( EnemyAttrs )
import Arkham.GameEnv
import Arkham.Message

class RunMessage a where
  runMessage :: Message -> a -> GameT a
