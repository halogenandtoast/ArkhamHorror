{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.GardenDistrict where

import Arkham.Import

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
  1
  (Static 0)
  Plus
  [Square, Plus]
  [NewOrleans]

instance HasModifiersFor env investigator GardenDistrict where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator GardenDistrict where
  getActions i NonFast (GardenDistrict attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions i NonFast attrs
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             (getId () i)
             (mkAbility (LocationSource locationId) 1 (ActionAbility 1 Nothing))
         | atLocation i attrs && hasActionsRemaining i Nothing locationTraits
         ]
  getActions i window (GardenDistrict attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env GardenDistrict where
  runMessage msg l@(GardenDistrict attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> l <$ unshiftMessage
      (BeginSkillTest
        iid
        source
        (toTarget attrs)
        Nothing
        SkillAgility
        7
        [Remember FoundAStrangeDoll]
        mempty
        mempty
        mempty
      )
    _ -> GardenDistrict <$> runMessage msg attrs
