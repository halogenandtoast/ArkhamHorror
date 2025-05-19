module Arkham.Location.Cards.AsylumHallsWesternPatientWing_168 (asylumHallsWesternPatientWing_168) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype AsylumHallsWesternPatientWing_168 = AsylumHallsWesternPatientWing_168 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsWesternPatientWing_168 :: LocationCard AsylumHallsWesternPatientWing_168
asylumHallsWesternPatientWing_168 =
  location AsylumHallsWesternPatientWing_168 Cards.asylumHallsWesternPatientWing_168 2 (Static 0)

instance HasAbilities AsylumHallsWesternPatientWing_168 where
  getAbilities (AsylumHallsWesternPatientWing_168 a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction (EnemyDefeated #after You ByAny $ EnemyWithTrait Lunatic)

instance RunMessage AsylumHallsWesternPatientWing_168 where
  runMessage msg l@(AsylumHallsWesternPatientWing_168 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> AsylumHallsWesternPatientWing_168 <$> liftRunMessage msg attrs
