module Arkham.Types.Location.Cards.GardenDistrict
  ( GardenDistrict(..)
  , gardenDistrict
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


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

newtype GardenDistrict = GardenDistrict LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gardenDistrict :: GardenDistrict
gardenDistrict = GardenDistrict $ baseAttrs
  "81008"
  (Name "Garden District" Nothing)
  EncounterSet.CurseOfTheRougarou
  1
  (Static 0)
  Plus
  [Square, Plus]
  [NewOrleans]

instance HasModifiersFor env GardenDistrict where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GardenDistrict where
  getActions iid NonFast (GardenDistrict attrs@LocationAttrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (LocationSource locationId)
            1
            (ActionAbility Nothing $ ActionCost 1)
          )
      | iid `member` locationInvestigators
      ]
  getActions i window (GardenDistrict attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env GardenDistrict where
  runMessage msg l@(GardenDistrict attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (BeginSkillTest iid source (toTarget attrs) Nothing SkillAgility 7)
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      l <$ unshiftMessage (Remember FoundAStrangeDoll)
    _ -> GardenDistrict <$> runMessage msg attrs
