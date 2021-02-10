module Arkham.Types.Location.Cards.HouseInTheReeds_211
  ( houseInTheReeds_211
  , HouseInTheReeds_211(..)
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


import Arkham.Types.Card.EncounterCardMatcher
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype HouseInTheReeds_211 = HouseInTheReeds_211 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

houseInTheReeds_211 :: HouseInTheReeds_211
houseInTheReeds_211 = HouseInTheReeds_211 $ baseAttrs
  "02211"
  (Name "House in the Reeds" Nothing)
  EncounterSet.BloodOnTheAltar
  1
  (PerPlayer 1)
  Squiggle
  [Diamond, Moon]
  [Dunwich]

instance HasModifiersFor env HouseInTheReeds_211 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HouseInTheReeds_211 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env HouseInTheReeds_211 where
  runMessage msg l@(HouseInTheReeds_211 attrs) = case msg of
    RevealLocation miid lid | lid == locationId attrs -> do
      iid <- maybe getLeadInvestigatorId pure miid
      unshiftMessage $ FindEncounterCard
        iid
        (toTarget attrs)
        (EncounterCardMatchByType (EnemyType, Just Nightgaunt))
      HouseInTheReeds_211 <$> runMessage msg attrs
    FoundEncounterCard _iid target card | isTarget attrs target -> do
      villageCommonsId <- fromJustNote "missing village commons"
        <$> getId (LocationWithTitle "Village Commons")
      l <$ unshiftMessage (SpawnEnemyAt (EncounterCard card) villageCommonsId)
    _ -> HouseInTheReeds_211 <$> runMessage msg attrs
