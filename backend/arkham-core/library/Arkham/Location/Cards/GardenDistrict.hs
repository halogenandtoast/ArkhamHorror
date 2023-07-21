module Arkham.Location.Cards.GardenDistrict (
  GardenDistrict (..),
  gardenDistrict,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (gardenDistrict)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.ScenarioLogKey
import Arkham.SkillType

newtype GardenDistrict = GardenDistrict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gardenDistrict :: LocationCard GardenDistrict
gardenDistrict = location GardenDistrict Cards.gardenDistrict 1 (Static 0)

instance HasAbilities GardenDistrict where
  getAbilities (GardenDistrict attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 1
      | locationRevealed attrs
      ]

instance RunMessage GardenDistrict where
  runMessage msg l@(GardenDistrict attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l
            <$ push
              (beginSkillTest iid source (toTarget attrs) SkillAgility 7)
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> l <$ push (Remember FoundAStrangeDoll)
    _ -> GardenDistrict <$> runMessage msg attrs
