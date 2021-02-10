module Arkham.Types.Investigator.Cards.DianaStanley where

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
 hiding (Cultist)

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype DianaStanley = DianaStanley InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env DianaStanley where
  getModifiersFor source target (DianaStanley attrs) =
    getModifiersFor source target attrs

dianaStanley :: DianaStanley
dianaStanley = DianaStanley $ baseAttrs
  "05004"
  "Diana Stanley"
  Mystic
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Cultist, SilverTwilight]

instance ActionRunner env => HasActions env DianaStanley where
  getActions i window (DianaStanley attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env DianaStanley where
  runMessage msg (DianaStanley attrs) = DianaStanley <$> runMessage msg attrs
