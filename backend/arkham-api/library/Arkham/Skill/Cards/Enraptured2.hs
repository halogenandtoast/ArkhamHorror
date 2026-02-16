module Arkham.Skill.Cards.Enraptured2 (enraptured2) where

import Arkham.Action qualified as Action
import Arkham.Asset.Uses qualified as Uses
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Enraptured2 = Enraptured2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enraptured2 :: SkillCard Enraptured2
enraptured2 = skill Enraptured2 Cards.enraptured2

instance RunMessage Enraptured2 where
  runMessage msg s@(Enraptured2 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n -> do
              when (st.action == Just Action.Investigate) do
                let passedMsg = PassedSkillTest st.investigator st.action st.source st.target st.kind n
                provideSkillTestResultOption attrs exclusions "Enraptured (2)" $ doStep 3 passedMsg
            _ -> pure ()
      pure s
    DoStep n msg'@(PassedSkillTest _ (Just Action.Investigate) _ (isTarget attrs -> True) _ _) -> do
      when (n > 0) do
        chargeAssets <-
          select
            $ AssetControlledBy (affectsOthers $ colocatedWith attrs.owner)
            <> AssetCanHaveUses Uses.Charge
        secretAssets <-
          select
            $ AssetControlledBy (affectsOthers $ colocatedWith attrs.owner)
            <> AssetCanHaveUses Uses.Secret
        unless (null chargeAssets && null secretAssets) do
          chooseOneM attrs.owner do
            targets chargeAssets \aid -> addUses attrs aid Uses.Charge 1
            targets secretAssets \aid -> addUses attrs aid Uses.Secret 1
          doStep (n - 1) msg'
      pure s
    _ -> Enraptured2 <$> liftRunMessage msg attrs
