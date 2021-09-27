module Arkham.Types.Location.Cards.GardenDistrict
  ( GardenDistrict(..)
  , gardenDistrict
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (gardenDistrict)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype GardenDistrict = GardenDistrict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gardenDistrict :: LocationCard GardenDistrict
gardenDistrict =
  location GardenDistrict Cards.gardenDistrict 1 (Static 0) Plus [Square, Plus]

instance HasAbilities GardenDistrict where
  getAbilities (GardenDistrict attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 1
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env GardenDistrict where
  runMessage msg l@(GardenDistrict attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push
        (BeginSkillTest iid source (toTarget attrs) Nothing SkillAgility 7)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember FoundAStrangeDoll)
    _ -> GardenDistrict <$> runMessage msg attrs
