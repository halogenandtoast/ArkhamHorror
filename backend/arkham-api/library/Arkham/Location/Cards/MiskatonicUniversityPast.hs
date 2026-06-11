module Arkham.Location.Cards.MiskatonicUniversityPast (miskatonicUniversityPast) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Scientist))

newtype MiskatonicUniversityPast = MiskatonicUniversityPast LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityPast :: LocationCard MiskatonicUniversityPast
miskatonicUniversityPast = location MiskatonicUniversityPast Cards.miskatonicUniversityPast 3 (PerPlayer 1)

instance HasAbilities MiskatonicUniversityPast where
  getAbilities (MiskatonicUniversityPast a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ ActionAbility []
      $ ActionCost 2
      <> SpendTokenCost Token.Seed (TargetIs $ toTarget a)

instance RunMessage MiskatonicUniversityPast where
  runMessage msg l@(MiskatonicUniversityPast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remember ATreeSeedHasBeenPlanted
      n <- selectCount $ AssetWithTrait Scientist <> AssetAt (be attrs)
      gainResources iid (attrs.ability 1) n
      pure l
    _ -> MiskatonicUniversityPast <$> liftRunMessage msg attrs
