module Arkham.Location.Cards.TreacherousPathPrecariousClimb (treacherousPathPrecariousClimb) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheWesternWall.Helpers (getLocationLevel)

newtype TreacherousPathPrecariousClimb = TreacherousPathPrecariousClimb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathPrecariousClimb :: LocationCard TreacherousPathPrecariousClimb
treacherousPathPrecariousClimb = location TreacherousPathPrecariousClimb Cards.treacherousPathPrecariousClimb 0 (Static 1)

instance HasAbilities TreacherousPathPrecariousClimb where
  getAbilities (TreacherousPathPrecariousClimb a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ Enters #after You (be a)

instance RunMessage TreacherousPathPrecariousClimb where
  runMessage msg l@(TreacherousPathPrecariousClimb attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      level <- getLocationLevel iid
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed level)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      push $ IncreaseFloodLevel attrs.id
      pure l
    _ -> TreacherousPathPrecariousClimb <$> liftRunMessage msg attrs
