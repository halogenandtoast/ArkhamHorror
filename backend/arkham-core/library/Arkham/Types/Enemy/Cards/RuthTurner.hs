module Arkham.Types.Enemy.Cards.RuthTurner where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype RuthTurner = RuthTurner EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruthTurner :: EnemyCard RuthTurner
ruthTurner = enemy RuthTurner Cards.ruthTurner
  $ (healthDamageL .~ 1)
  . (fightL .~ 2)
  . (healthL .~ Static 4)
  . (evadeL .~ 5)
  . (spawnAtL ?~ LocationWithTitle "St. Mary's Hospital")

instance HasModifiersFor env RuthTurner where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env RuthTurner where
  getActions i window (RuthTurner attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RuthTurner where
  runMessage msg e@(RuthTurner attrs) = case msg of
    EnemyEvaded _ eid | eid == enemyId attrs ->
      e <$ unshiftMessage (AddToVictory $ toTarget attrs)
    _ -> RuthTurner <$> runMessage msg attrs
