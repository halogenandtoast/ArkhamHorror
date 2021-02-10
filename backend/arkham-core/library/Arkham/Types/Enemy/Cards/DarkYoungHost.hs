module Arkham.Types.Enemy.Cards.DarkYoungHost where

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
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype DarkYoungHost = DarkYoungHost EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkYoungHost :: EnemyId -> DarkYoungHost
darkYoungHost uuid =
  DarkYoungHost
    $ baseAttrs uuid "81033"
    $ (healthDamageL .~ 2)
    . (sanityDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 5)
    . (evadeL .~ 2)

instance HasModifiersFor env DarkYoungHost where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DarkYoungHost where
  getActions i window (DarkYoungHost attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env DarkYoungHost where
  runMessage msg e@(DarkYoungHost attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId -> do
      leadInvestigatorId <- getLeadInvestigatorId
      bayouLocations <- getSetList [Bayou]
      e <$ spawnAtOneOf leadInvestigatorId enemyId bayouLocations
    PlaceClues (LocationTarget lid) n | lid == enemyLocation -> do
      unshiftMessage $ RemoveClues (LocationTarget lid) n
      pure . DarkYoungHost $ attrs & cluesL +~ n
    When (EnemyDefeated eid _ _ _ _ _) | eid == enemyId ->
      e <$ unshiftMessage (PlaceClues (LocationTarget enemyLocation) enemyClues)
    _ -> DarkYoungHost <$> runMessage msg attrs
