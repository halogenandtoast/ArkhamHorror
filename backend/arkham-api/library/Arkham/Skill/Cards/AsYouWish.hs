module Arkham.Skill.Cards.AsYouWish (asYouWish) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype AsYouWish = AsYouWish SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asYouWish :: SkillCard AsYouWish
asYouWish = skill AsYouWish Cards.asYouWish

instance RunMessage AsYouWish where
  runMessage msg s@(AsYouWish attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              provideSkillTestResultOption attrs exclusions "AsYouWish" $ drawCardsIfCan st.investigator attrs 1
            FailedBy {} -> do
              provideSkillTestResultOption attrs exclusions "AsYouWish" $ drawCardsIfCan attrs.owner attrs 1
            _ -> pure ()
      pure s
    _ -> AsYouWish <$> liftRunMessage msg attrs
