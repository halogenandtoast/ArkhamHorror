module Arkham.Types.Enemy.Cards.EmergentMonstrosity
  ( EmergentMonstrosity(..)
  , emergentMonstrosity
  ) where

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

newtype EmergentMonstrosity = EmergentMonstrosity EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergentMonstrosity :: EnemyId -> EmergentMonstrosity
emergentMonstrosity uuid =
  EmergentMonstrosity
    $ baseAttrs uuid "02183"
    $ (healthDamageL .~ 2)
    . (sanityDamageL .~ 2)
    . (fightL .~ 4)
    . (healthL .~ Static 5)
    . (evadeL .~ 4)

instance HasModifiersFor env EmergentMonstrosity where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env EmergentMonstrosity where
  getActions i window (EmergentMonstrosity attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env EmergentMonstrosity where
  runMessage msg (EmergentMonstrosity attrs@EnemyAttrs {..}) = case msg of

    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      lid <- getId @LocationId iid
      spawnLocation <- fromMaybe lid <$> getId (RightOf, lid)
      unshiftMessage (EnemySpawn (Just iid) spawnLocation enemyId)
      pure . EmergentMonstrosity $ attrs & exhaustedL .~ True
    _ -> EmergentMonstrosity <$> runMessage msg attrs
