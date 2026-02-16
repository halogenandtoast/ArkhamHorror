module Arkham.Skill.Cards.WatchThis (watchThis) where

import Arkham.Calculation
import Arkham.Cost
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

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
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n | n >= 1 -> do
              provideSkillTestResultOption attrs exclusions "Watch This" do
                gainResources st.investigator attrs $ 2 * (skillAdditionalPayment attrs).resources
            _ -> pure ()
      pure s
    _ -> WatchThis <$> liftRunMessage msg attrs
