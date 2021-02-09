module Arkham.Types.Enemy.Cards.MarshGug where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype MarshGug = MarshGug EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marshGug :: EnemyId -> MarshGug
marshGug uuid =
  MarshGug
    $ baseAttrs uuid "81032"
    $ (healthDamageL .~ 2)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 3)

instance HasModifiersFor env MarshGug where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MarshGug where
  getActions i window (MarshGug attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env MarshGug where
  runMessage msg e@(MarshGug attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId -> do
      leadInvestigatorId <- getLeadInvestigatorId
      bayouLocations <- getSetList [Bayou]
      e <$ spawnAtOneOf leadInvestigatorId enemyId bayouLocations
    _ -> MarshGug <$> runMessage msg attrs
