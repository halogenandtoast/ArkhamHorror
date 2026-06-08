module Arkham.Skill.Cards.WatchThis (watchThis) where

import Arkham.Calculation
import Arkham.Capability
import Arkham.Cost
import Arkham.Matcher
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
      when ((skillAdditionalPayment attrs).resources > 0) do
        skillTestCardOptionEdit attrs (optionWhenExists $ investigator_ $ can.gain.resources iid)
          $ doStep 1 msg
      pure s
    DoStep 1 (PassedSkillTest iid _ _ (isTarget attrs -> True) _ _) -> do
      gainResources iid attrs $ 2 * (skillAdditionalPayment attrs).resources
      pure s
    _ -> WatchThis <$> liftRunMessage msg attrs
