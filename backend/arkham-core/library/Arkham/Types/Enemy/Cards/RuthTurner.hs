module Arkham.Types.Enemy.Cards.RuthTurner where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype RuthTurner = RuthTurner EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruthTurner :: EnemyId -> RuthTurner
ruthTurner uuid =
  RuthTurner
    $ baseAttrs uuid "01141"
    $ (healthDamageL .~ 1)
    . (fightL .~ 2)
    . (healthL .~ Static 4)
    . (evadeL .~ 5)
    . (uniqueL .~ True)
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
