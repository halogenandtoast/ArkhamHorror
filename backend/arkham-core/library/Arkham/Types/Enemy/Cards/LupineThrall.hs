module Arkham.Types.Enemy.Cards.LupineThrall
  ( LupineThrall(..)
  , lupineThrall
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

newtype LupineThrall = LupineThrall EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lupineThrall :: EnemyId -> LupineThrall
lupineThrall uuid =
  LupineThrall
    $ baseAttrs uuid "02095"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 3)
    . (evadeL .~ 4)
    . (preyL .~ LowestSkill SkillAgility)

instance ActionRunner env => HasActions env LupineThrall where
  getActions i window (LupineThrall attrs) = getActions i window attrs

instance HasModifiersFor env LupineThrall where
  getModifiersFor = noModifiersFor

instance EnemyRunner env => RunMessage env LupineThrall where
  runMessage msg e@(LupineThrall attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      farthestLocations <- map unFarthestLocationId <$> getSetList iid
      e <$ spawnAtOneOf iid eid farthestLocations
    _ -> LupineThrall <$> runMessage msg attrs
