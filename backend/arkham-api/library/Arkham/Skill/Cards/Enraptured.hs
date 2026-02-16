module Arkham.Skill.Cards.Enraptured (enraptured) where

import Arkham.Action qualified as Action
import Arkham.Asset.Uses qualified as Uses
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Enraptured = Enraptured SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enraptured :: SkillCard Enraptured
enraptured = skill Enraptured Cards.enraptured

instance RunMessage Enraptured where
  runMessage msg s@(Enraptured attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              when (st.action == Just Action.Investigate) do
                chargeAssets <- select $ assetControlledBy attrs.owner <> AssetCanHaveUses Uses.Charge
                secretAssets <- select $ assetControlledBy attrs.owner <> AssetCanHaveUses Uses.Secret
                provideSkillTestResultOption attrs exclusions "Enraptured" do
                  chooseTargetM attrs.owner (nub $ chargeAssets <> secretAssets) \aid -> do
                    chooseOrRunOneM attrs.owner do
                      when (aid `elem` chargeAssets) do
                        labeled "Charge" $ addUses attrs aid Uses.Charge 1
                      when (aid `elem` secretAssets) do
                        labeled "Secret" $ addUses attrs aid Uses.Secret 1
            _ -> pure ()
      pure s
    _ -> Enraptured <$> liftRunMessage msg attrs
