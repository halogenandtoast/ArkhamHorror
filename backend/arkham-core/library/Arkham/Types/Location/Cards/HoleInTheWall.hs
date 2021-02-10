module Arkham.Types.Location.Cards.HoleInTheWall where

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

newtype HoleInTheWall = HoleInTheWall LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holeInTheWall :: HoleInTheWall
holeInTheWall = HoleInTheWall $ baseAttrs
  "50017"
  (Name "Hallway" Nothing)
  EncounterSet.ReturnToTheGathering
  1
  (Static 0)
  Square
  [T, Triangle, Plus, Diamond]
  mempty

instance HasModifiersFor env HoleInTheWall where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HoleInTheWall where
  getActions i window (HoleInTheWall attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env HoleInTheWall where
  runMessage msg (HoleInTheWall attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessages
        [ PlaceLocationMatching (LocationWithTitle "Attic")
        , PlaceLocationMatching (LocationWithTitle "Cellar")
        , PlaceLocationMatching (LocationWithTitle "Parlor")
        ]
      HoleInTheWall <$> runMessage msg attrs
    _ -> HoleInTheWall <$> runMessage msg attrs
