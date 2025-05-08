module Arkham.Location.Cards.AsylumHallsEasternPatientWing_170 (asylumHallsEasternPatientWing_170) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype AsylumHallsEasternPatientWing_170 = AsylumHallsEasternPatientWing_170 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsEasternPatientWing_170
  :: LocationCard AsylumHallsEasternPatientWing_170
asylumHallsEasternPatientWing_170 =
  location AsylumHallsEasternPatientWing_170 Cards.asylumHallsEasternPatientWing_170 3 (Static 0)

instance HasAbilities AsylumHallsEasternPatientWing_170 where
  getAbilities (AsylumHallsEasternPatientWing_170 attrs) =
    extendRevealed1 attrs
      $ notSkillTestAbility
      $ restricted attrs 1 (Here <> exists (EnemyAt YourLocation <> EnemyWithTrait Lunatic))
      $ evadeAction (HorrorCost (toSource attrs) YouTarget 1)

instance RunMessage AsylumHallsEasternPatientWing_170 where
  runMessage msg l@(AsylumHallsEasternPatientWing_170 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt YourLocation <> EnemyWithTrait Lunatic
      chooseTargetM iid enemies (automaticallyEvadeEnemy iid)
      pure l
    _ -> AsylumHallsEasternPatientWing_170 <$> liftRunMessage msg attrs
