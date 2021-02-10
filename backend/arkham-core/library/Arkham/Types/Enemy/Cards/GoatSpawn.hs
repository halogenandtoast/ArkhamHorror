module Arkham.Types.Enemy.Cards.GoatSpawn where

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

newtype GoatSpawn = GoatSpawn EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goatSpawn :: EnemyId -> GoatSpawn
goatSpawn uuid =
  GoatSpawn
    $ baseAttrs uuid "01180"
    $ (healthDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 3)
    . (evadeL .~ 2)

instance HasModifiersFor env GoatSpawn where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GoatSpawn where
  getActions i window (GoatSpawn attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GoatSpawn where
  runMessage msg (GoatSpawn attrs@EnemyAttrs {..}) = case msg of
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId -> do
      investigatorIds <- getSetList enemyLocation
      unshiftMessages
        [ InvestigatorAssignDamage iid (EnemySource eid) DamageAny 0 1
        | iid <- investigatorIds
        ]
      GoatSpawn <$> runMessage msg attrs
    _ -> GoatSpawn <$> runMessage msg attrs
