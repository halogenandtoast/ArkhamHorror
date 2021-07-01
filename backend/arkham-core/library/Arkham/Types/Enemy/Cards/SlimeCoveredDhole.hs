module Arkham.Types.Enemy.Cards.SlimeCoveredDhole
  ( SlimeCoveredDhole(..)
  , slimeCoveredDhole
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Trait

newtype SlimeCoveredDhole = SlimeCoveredDhole EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slimeCoveredDhole :: EnemyCard SlimeCoveredDhole
slimeCoveredDhole = enemy SlimeCoveredDhole Cards.slimeCoveredDhole
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 1)
  . (fightL .~ 2)
  . (healthL .~ Static 3)
  . (evadeL .~ 3)
  . (preyL .~ LowestRemainingHealth)

instance HasModifiersFor env SlimeCoveredDhole where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env SlimeCoveredDhole where
  getActions i window (SlimeCoveredDhole attrs) = getActions i window attrs

bayouLocations
  :: (MonadReader env m, HasSet LocationId env [Trait])
  => m (HashSet LocationId)
bayouLocations = getSet [Bayou]

nonBayouLocations
  :: ( MonadReader env m
     , HasSet LocationId env ()
     , HasSet LocationId env [Trait]
     )
  => m (HashSet LocationId)
nonBayouLocations = difference <$> getLocationSet <*> bayouLocations

instance (EnemyRunner env) => RunMessage env SlimeCoveredDhole where
  runMessage msg e@(SlimeCoveredDhole attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      spawnLocations <- setToList <$> nonBayouLocations
      e <$ spawnAtOneOf iid enemyId spawnLocations
    EnemyMove eid _ lid | eid == enemyId -> do
      investigatorIds <- getSetList @InvestigatorId lid
      e <$ unshiftMessages
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
        | iid <- investigatorIds
        ]
    _ -> SlimeCoveredDhole <$> runMessage msg attrs
