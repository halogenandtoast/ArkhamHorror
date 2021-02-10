module Arkham.Types.Investigator.Cards.JennyBarnes where

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


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype JennyBarnes = JennyBarnes InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env JennyBarnes where
  getModifiersFor source target (JennyBarnes attrs) =
    getModifiersFor source target attrs

jennyBarnes :: JennyBarnes
jennyBarnes = JennyBarnes $ baseAttrs
  "02003"
  "Jenny Barnes"
  Rogue
  Stats
    { health = 8
    , sanity = 7
    , willpower = 3
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Drifter]

instance ActionRunner env => HasActions env JennyBarnes where
  getActions i window (JennyBarnes attrs) = getActions i window attrs

instance HasTokenValue env JennyBarnes where
  getTokenValue (JennyBarnes attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier $ investigatorResources attrs)
  getTokenValue (JennyBarnes attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env JennyBarnes where
  runMessage msg (JennyBarnes attrs) = case msg of
    AllDrawCardAndResource | not (attrs ^. defeatedL || attrs ^. resignedL) ->
      JennyBarnes <$> runMessage msg (attrs & resourcesL +~ 1)
    _ -> JennyBarnes <$> runMessage msg attrs
