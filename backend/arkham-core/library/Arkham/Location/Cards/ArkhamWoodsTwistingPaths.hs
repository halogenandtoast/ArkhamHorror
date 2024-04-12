module Arkham.Location.Cards.ArkhamWoodsTwistingPaths where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (arkhamWoodsTwistingPaths)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (getBatchId)

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: LocationCard ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths = location ArkhamWoodsTwistingPaths Cards.arkhamWoodsTwistingPaths 3 (PerPlayer 1)

instance HasAbilities ArkhamWoodsTwistingPaths where
  getAbilities (ArkhamWoodsTwistingPaths attrs) =
    withRevealedAbilities attrs [forcedAbility attrs 1 $ Leaves #when You $ be attrs]

instance RunMessage ArkhamWoodsTwistingPaths where
  runMessage msg l@(ArkhamWoodsTwistingPaths attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) (BatchTarget batchId) #intellect (Fixed 3)
      pure l
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just (BatchTarget batchId) -> push $ CancelBatch batchId
        _ -> error "Invalid target"
      pure l
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
