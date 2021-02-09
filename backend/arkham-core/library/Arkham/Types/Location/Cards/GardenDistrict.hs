module Arkham.Types.Location.Cards.GardenDistrict
  ( GardenDistrict(..)
  , gardenDistrict
  )
where


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
