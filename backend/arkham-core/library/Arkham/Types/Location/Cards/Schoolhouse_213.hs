module Arkham.Types.Location.Cards.Schoolhouse_213
  ( schoolhouse_213
  , Schoolhouse_213(..)
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


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Schoolhouse_213 = Schoolhouse_213 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolhouse_213 :: Schoolhouse_213
schoolhouse_213 = Schoolhouse_213 $ baseAttrs
  "02213"
  (Name "Schoolhouse" Nothing)
  EncounterSet.BloodOnTheAltar
  4
  (Static 1)
  Moon
  [Plus, Squiggle, Circle]
  [Dunwich]

instance HasModifiersFor env Schoolhouse_213 where
  getModifiersFor = noModifiersFor


instance ActionRunner env => HasActions env Schoolhouse_213 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env Schoolhouse_213 where
  runMessage msg l@(Schoolhouse_213 attrs) = case msg of
    -- Cannot discover clues except by investigating so we just noop
    DiscoverCluesAtLocation _ lid _ Nothing | lid == locationId attrs -> pure l
    _ -> Schoolhouse_213 <$> runMessage msg attrs
