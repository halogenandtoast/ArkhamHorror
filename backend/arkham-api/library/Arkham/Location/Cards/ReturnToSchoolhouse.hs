module Arkham.Location.Cards.ReturnToSchoolhouse (returnToSchoolhouse) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToSchoolhouse = ReturnToSchoolhouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToSchoolhouse :: LocationCard ReturnToSchoolhouse
returnToSchoolhouse =
  locationWith ReturnToSchoolhouse Cards.returnToSchoolhouse 4 (PerPlayer 1) (labelL .~ "schoolhouse")

instance HasAbilities ReturnToSchoolhouse where
  getAbilities (ReturnToSchoolhouse a) =
    withDrawCardUnderneathAction a
      <> extendRevealed1
        a
        (mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1))

instance RunMessage ReturnToSchoolhouse where
  runMessage msg l@(ReturnToSchoolhouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findAndDrawEncounterCard iid Enemies.servantOfManyMouths
      pure l
    _ -> ReturnToSchoolhouse <$> liftRunMessage msg attrs
