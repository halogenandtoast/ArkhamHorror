module Arkham.Skill.Cards.WatchThis3 (watchThis3) where

import Arkham.Calculation
import Arkham.Cost
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

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
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n | n >= 1 -> do
              provideSkillTestResultOption attrs exclusions "Watch This (3)" do
                gainResources st.investigator attrs $ 2 * (skillAdditionalPayment attrs).resources
            _ -> pure ()
      pure s
    _ -> WatchThis3 <$> liftRunMessage msg attrs
