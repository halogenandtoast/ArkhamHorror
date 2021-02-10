module Arkham.Types.Location.Cards.ReturnToAttic where

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


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype ReturnToAttic = ReturnToAttic LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToAttic :: ReturnToAttic
returnToAttic = ReturnToAttic $ baseAttrs
  "50018"
  (Name "Attic" Nothing)
  EncounterSet.ReturnToTheGathering
  3
  (PerPlayer 1)
  Triangle
  [Square, Moon]
  mempty

instance HasModifiersFor env ReturnToAttic where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ReturnToAttic where
  getActions i window (ReturnToAttic attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ReturnToAttic where
  runMessage msg (ReturnToAttic attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessage (PlaceLocation "50019")
      ReturnToAttic <$> runMessage msg attrs
    _ -> ReturnToAttic <$> runMessage msg attrs
