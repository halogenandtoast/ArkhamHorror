module Arkham.Types.Enemy.Cards.TheExperiment where

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
import Arkham.Types.Game.Helpers

newtype TheExperiment = TheExperiment EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theExperiment :: EnemyId -> TheExperiment
theExperiment uuid =
  TheExperiment
    $ baseAttrs uuid "02058"
    $ (healthDamageL .~ 2)
    . (sanityDamageL .~ 2)
    . (fightL .~ 4)
    . (healthL .~ Static 7)
    . (evadeL .~ 2)
    . (uniqueL .~ True)

instance HasCount PlayerCount env () => HasModifiersFor env TheExperiment where
  getModifiersFor _ target (TheExperiment attrs) | isTarget attrs target = do
    modifier <- getPlayerCountValue (PerPlayer 3)
    pure $ toModifiers attrs [HealthModifier modifier]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env TheExperiment where
  getActions i window (TheExperiment attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env TheExperiment where
  runMessage msg (TheExperiment attrs) = case msg of
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId attrs -> do
      actId <- fromJustNote "missing act" . headMay <$> getSetList ()
      unshiftMessage (AdvanceAct actId (toSource attrs))
      TheExperiment <$> runMessage msg attrs
    BeginEnemy -> TheExperiment <$> runMessage ReadyExhausted attrs
    _ -> TheExperiment <$> runMessage msg attrs
