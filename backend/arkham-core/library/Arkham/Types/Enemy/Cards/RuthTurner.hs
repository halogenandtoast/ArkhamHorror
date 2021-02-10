module Arkham.Types.Enemy.Cards.RuthTurner where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

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

instance HasModifiersFor env RuthTurner where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env RuthTurner where
  getActions i window (RuthTurner attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RuthTurner where
  runMessage msg e@(RuthTurner attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid (LocationWithTitle "St. Mary's Hospital")
    EnemyEvaded _ eid | eid == enemyId ->
      e <$ unshiftMessage (AddToVictory (EnemyTarget enemyId))
    _ -> RuthTurner <$> runMessage msg attrs
