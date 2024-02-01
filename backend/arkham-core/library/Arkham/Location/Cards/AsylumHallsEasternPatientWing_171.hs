module Arkham.Location.Cards.AsylumHallsEasternPatientWing_171 (
  asylumHallsEasternPatientWing_171,
  AsylumHallsEasternPatientWing_171 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype AsylumHallsEasternPatientWing_171 = AsylumHallsEasternPatientWing_171 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

asylumHallsEasternPatientWing_171
  :: LocationCard AsylumHallsEasternPatientWing_171
asylumHallsEasternPatientWing_171 =
  location
    AsylumHallsEasternPatientWing_171
    Cards.asylumHallsEasternPatientWing_171
    2
    (PerPlayer 1)

instance HasAbilities AsylumHallsEasternPatientWing_171 where
  getAbilities (AsylumHallsEasternPatientWing_171 attrs) =
    withRevealedAbilities
      attrs
      [restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds #after You]

instance RunMessage AsylumHallsEasternPatientWing_171 where
  runMessage msg l@(AsylumHallsEasternPatientWing_171 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll [assignHorror iid source 1, takeResources iid source 1]
      pure l
    _ -> AsylumHallsEasternPatientWing_171 <$> runMessage msg attrs
