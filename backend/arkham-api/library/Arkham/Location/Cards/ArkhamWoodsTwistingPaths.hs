module Arkham.Location.Cards.ArkhamWoodsTwistingPaths (arkhamWoodsTwistingPaths) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (arkhamWoodsTwistingPaths)
import Arkham.Location.Import.Lifted
import Arkham.Matcher.Base
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Window
import Arkham.Window (getBatchId)

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: LocationCard ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths = location ArkhamWoodsTwistingPaths Cards.arkhamWoodsTwistingPaths 3 (PerPlayer 1)

instance HasAbilities ArkhamWoodsTwistingPaths where
  getAbilities (ArkhamWoodsTwistingPaths attrs) =
    extendRevealed1 attrs
      $ skillTestAbility
      $ forcedAbility attrs 1
      $ WouldMove #when You #any (be attrs) Anywhere

instance RunMessage ArkhamWoodsTwistingPaths where
  runMessage msg l@(ArkhamWoodsTwistingPaths attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) (BatchTarget batchId) #intellect (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      cancelMovement (attrs.ability 1) iid
      pure l
    _ -> ArkhamWoodsTwistingPaths <$> liftRunMessage msg attrs
