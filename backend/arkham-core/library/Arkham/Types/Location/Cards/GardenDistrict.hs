module Arkham.Types.Location.Cards.GardenDistrict
  ( GardenDistrict(..)
  , gardenDistrict
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (gardenDistrict)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype GardenDistrict = GardenDistrict LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gardenDistrict :: LocationId -> GardenDistrict
gardenDistrict = GardenDistrict . baseAttrs
  Cards.gardenDistrict
  1
  (Static 0)
  Plus
  [Square, Plus]

instance HasModifiersFor env GardenDistrict where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GardenDistrict where
  getActions iid NonFast (GardenDistrict attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
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
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l
      <$ unshiftMessage (Remember FoundAStrangeDoll)
    _ -> GardenDistrict <$> runMessage msg attrs
