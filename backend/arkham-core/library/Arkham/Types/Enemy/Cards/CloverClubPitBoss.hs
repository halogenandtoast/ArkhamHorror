module Arkham.Types.Enemy.Cards.CloverClubPitBoss
  ( CloverClubPitBoss(..)
  , cloverClubPitBoss
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

newtype CloverClubPitBoss = CloverClubPitBoss EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubPitBoss :: EnemyId -> CloverClubPitBoss
cloverClubPitBoss uuid =
  CloverClubPitBoss
    $ baseAttrs uuid "02078"
    $ (healthDamageL .~ 2)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 3)
    . (preyL .~ HighestSkill SkillIntellect)

instance HasModifiersFor env CloverClubPitBoss where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CloverClubPitBoss where
  getActions i window (CloverClubPitBoss attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env CloverClubPitBoss where
  runMessage msg e@(CloverClubPitBoss attrs@EnemyAttrs {..}) = case msg of
    After (GainClues iid n) | n > 0 -> do
      lid <- getId iid
      e <$ when
        (lid == enemyLocation)
        (unshiftMessages
        $ [ Ready (toTarget attrs) | enemyExhausted ]
        <> [ EnemyEngageInvestigator enemyId iid
           , EnemyAttackIfEngaged enemyId (Just iid)
           ]
        )
    _ -> CloverClubPitBoss <$> runMessage msg attrs
