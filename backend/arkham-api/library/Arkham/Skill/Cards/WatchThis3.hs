module Arkham.Skill.Cards.WatchThis3 (watchThis3) where

import Arkham.Calculation
import Arkham.Cost
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype WatchThis3 = WatchThis3 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchThis3 :: SkillCard WatchThis3
watchThis3 =
  skillWith WatchThis3 Cards.watchThis3
    $ additionalCostL
    ?~ UpTo (Fixed 3) (ResourceCost 1)

instance RunMessage WatchThis3 where
  runMessage msg s@(WatchThis3 attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 1 -> do
      skillTestResultOption "Watch This (3)" do
        gainResources iid attrs $ 2 * (skillAdditionalPayment attrs).resources
      pure s
    _ -> WatchThis3 <$> liftRunMessage msg attrs
