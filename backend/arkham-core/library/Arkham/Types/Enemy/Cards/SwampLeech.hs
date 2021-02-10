module Arkham.Types.Enemy.Cards.SwampLeech
  ( SwampLeech(..)
  , swampLeech
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype SwampLeech = SwampLeech EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swampLeech :: EnemyId -> SwampLeech
swampLeech uuid =
  SwampLeech
    $ baseAttrs uuid "81023"
    $ (healthDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 1)
    . (evadeL .~ 0)

instance HasModifiersFor env SwampLeech where
  getModifiersFor = noModifiersFor

isEvade :: Message -> Bool
isEvade = \case
  EvadeEnemy{} -> True
  _ -> False

instance ActionRunner env => HasActions env SwampLeech where
  getActions i window (SwampLeech attrs) = do
    actions' <- getActions i window attrs
    pure $ filter (not . isEvade) actions'

instance EnemyRunner env => RunMessage env SwampLeech where
  runMessage msg e@(SwampLeech attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      bayouLocations <- getSetList @LocationId [Bayou]
      e <$ spawnAtOneOf iid enemyId bayouLocations
    EnemyMove eid _ lid | eid == enemyId -> do
      bayouLocations <- getSetList @LocationId [Bayou]
      e <$ when
        (lid `notElem` bayouLocations)
        (unshiftMessage $ Discard (EnemyTarget enemyId))
    _ -> SwampLeech <$> runMessage msg attrs
