module Arkham.Skill.Cards.EldritchWhispers1 (eldritchWhispers1) where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher hiding (RevealChaosToken, SkillTestEnded)
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype EldritchWhispers1 = EldritchWhispers1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchWhispers1 :: SkillCard EldritchWhispers1
eldritchWhispers1 = skill EldritchWhispers1 Cards.eldritchWhispers1

instance RunMessage EldritchWhispers1 where
  runMessage msg s@(EldritchWhispers1 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill _iid skid | skid == attrs.id -> do
      withSkillTest \sid -> do
        onRevealChaosTokenEffect sid IsSymbol attrs attrs do
          do_ msg
      EldritchWhispers1 <$> liftRunMessage msg attrs
    Do msg'@(InvestigatorCommittedSkill _iid sid) | sid == attrs.id -> do
      doStep 2 msg'
      pure s
    DoStep n msg'@(InvestigatorCommittedSkill iid sid) | sid == attrs.id && n > 0 -> do
      chargeAssets <- select $ assetControlledBy iid <> AssetCanHaveUses Uses.Charge
      secretAssets <- select $ assetControlledBy iid <> AssetCanHaveUses Uses.Secret
      let allAssets = nub $ chargeAssets <> secretAssets
      unless (null allAssets) do
        chooseOneM iid do
          labeled "Done placing tokens" nothing
          targets allAssets \aid -> do
            chooseOrRunOneM iid do
              when (aid `elem` chargeAssets) $ labeled "Charge" $ addUses attrs aid Uses.Charge 1
              when (aid `elem` secretAssets) $ labeled "Secret" $ addUses attrs aid Uses.Secret 1
            doStep (n - 1) msg'
      pure s
    _ -> EldritchWhispers1 <$> liftRunMessage msg attrs
