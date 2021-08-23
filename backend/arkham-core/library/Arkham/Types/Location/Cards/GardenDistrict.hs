module Arkham.Types.Location.Cards.GardenDistrict
  ( GardenDistrict(..)
  , gardenDistrict
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (gardenDistrict)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype GardenDistrict = GardenDistrict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gardenDistrict :: LocationCard GardenDistrict
gardenDistrict =
  location GardenDistrict Cards.gardenDistrict 1 (Static 0) Plus [Square, Plus]

instance HasAbilities env GardenDistrict where
  getAbilities iid window@(Window Timing.When NonFast) (GardenDistrict attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseAbilities iid window attrs $ pure
      [ locationAbility
          (mkAbility attrs 1 $ ActionAbility Nothing $ ActionCost 1)
      ]
  getAbilities i window (GardenDistrict attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env GardenDistrict where
  runMessage msg l@(GardenDistrict attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push
        (BeginSkillTest iid source (toTarget attrs) Nothing SkillAgility 7)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember FoundAStrangeDoll)
    _ -> GardenDistrict <$> runMessage msg attrs
