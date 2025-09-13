module Arkham.Skill.Cards.WatchThis (watchThis) where

import Arkham.Calculation
import Arkham.Cost
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype WatchThis = WatchThis SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchThis :: SkillCard WatchThis
watchThis =
  skillWith WatchThis Cards.watchThis
    $ additionalCostL
    ?~ UpTo (Fixed 3) (ResourceCost 1)

instance RunMessage WatchThis where
  runMessage msg s@(WatchThis attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 1 -> do
      skillTestResultOption "Watch This" do
        gainResources iid attrs $ 2 * (skillAdditionalPayment attrs).resources
      pure s
    _ -> WatchThis <$> liftRunMessage msg attrs
