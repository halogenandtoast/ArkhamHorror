module Arkham.Types.Enemy.Cards.ConglomerationOfSpheres where

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
import Arkham.Types.Trait

newtype ConglomerationOfSpheres = ConglomerationOfSpheres EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conglomerationOfSpheres :: EnemyId -> ConglomerationOfSpheres
conglomerationOfSpheres uuid =
  ConglomerationOfSpheres
    $ baseAttrs uuid "02103"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 1)
    . (healthL .~ Static 6)
    . (evadeL .~ 4)
    . (preyL .~ LowestSkill SkillWillpower)

instance HasModifiersFor env ConglomerationOfSpheres where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ConglomerationOfSpheres where
  getActions i window (ConglomerationOfSpheres attrs) =
    getActions i window attrs

instance EnemyRunner env => RunMessage env ConglomerationOfSpheres where
  runMessage msg e@(ConglomerationOfSpheres attrs@EnemyAttrs {..}) = case msg of
    After (FightEnemy _ eid source _ _) | eid == enemyId -> do
      traits <- getSet source
      e <$ when
        (Melee `member` traits)
        (unshiftMessage $ Discard $ sourceToTarget source)
    _ -> ConglomerationOfSpheres <$> runMessage msg attrs
