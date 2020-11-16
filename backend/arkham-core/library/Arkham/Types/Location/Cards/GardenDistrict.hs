{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.GardenDistrict where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

newtype GardenDistrict = GardenDistrict Attrs
  deriving newtype (Show, ToJSON, FromJSON)

gardenDistrict :: GardenDistrict
gardenDistrict = GardenDistrict $ baseAttrs
  "81008"
  "Garden District"
  EncounterSet.CurseOfTheRougarou
  1
  (Static 0)
  Plus
  [Square, Plus]
  [NewOrleans]

instance HasModifiersFor env GardenDistrict where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GardenDistrict where
  getActions iid NonFast (GardenDistrict attrs@Attrs {..}) | locationRevealed =
    do
      baseActions <- getActions iid NonFast attrs
      hasActionsRemaining <- getHasActionsRemaining
        iid
        Nothing
        (setToList locationTraits)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction
               iid
               (mkAbility
                 (LocationSource locationId)
                 1
                 (ActionAbility 1 Nothing)
               )
           | iid `member` locationInvestigators && hasActionsRemaining
           ]
  getActions i window (GardenDistrict attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env GardenDistrict where
  runMessage msg l@(GardenDistrict attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> l <$ unshiftMessage
      (BeginSkillTest iid source (toTarget attrs) Nothing SkillAgility 7)
    PassedSkillTest _ _ source _ _ | isSource attrs source ->
      l <$ unshiftMessage (Remember FoundAStrangeDoll)
    _ -> GardenDistrict <$> runMessage msg attrs
