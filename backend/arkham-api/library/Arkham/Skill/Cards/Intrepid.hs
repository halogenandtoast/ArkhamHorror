module Arkham.Skill.Cards.Intrepid (intrepid) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Id
import Arkham.Message
import Arkham.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Intrepid = Intrepid SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intrepid :: SkillCard Intrepid
intrepid = skill Intrepid Cards.intrepid

instance RunMessage Intrepid where
  runMessage msg s@(Intrepid attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      intrepidAsset <- genPlayerCardWith Assets.intrepid (setOriginalCardCode attrs)
      let assetId = AssetId $ unSkillId $ toId attrs
      skillTestResultOption "Intrepid" do
        removeSkill attrs
        createAssetWithIdAt assetId intrepidAsset (InPlayArea iid)
      pure s
    _ -> Intrepid <$> liftRunMessage msg attrs
