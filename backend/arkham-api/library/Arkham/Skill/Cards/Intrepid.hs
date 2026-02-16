module Arkham.Skill.Cards.Intrepid (intrepid) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Id
import Arkham.Message
import Arkham.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Intrepid = Intrepid SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intrepid :: SkillCard Intrepid
intrepid = skill Intrepid Cards.intrepid

instance RunMessage Intrepid where
  runMessage msg s@(Intrepid attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              intrepidAsset <- genPlayerCardWith Assets.intrepid (setOriginalCardCode attrs)
              let assetId = AssetId $ unSkillId $ toId attrs
              provideSkillTestResultOption attrs exclusions "Intrepid" do
                removeSkill attrs
                createAssetWithIdAt assetId intrepidAsset (InPlayArea st.investigator)
            _ -> pure ()
      pure s
    _ -> Intrepid <$> liftRunMessage msg attrs
