module Arkham.Skill.Cards.Unrelenting1 (unrelenting1) where

import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Unrelenting1 = Unrelenting1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unrelenting1 :: SkillCard Unrelenting1
unrelenting1 = skill Unrelenting1 Cards.unrelenting1

instance RunMessage Unrelenting1 where
  runMessage msg s@(Unrelenting1 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == attrs.id -> do
      tokens <- getOnlyChaosTokensInBag
      focusChaosTokens tokens \unfocus -> do
        chooseUpToNM iid 3 "Done sealing tokens" do
          for_ (filter ((/= #autofail) . (.face)) tokens) \token -> do
            targeting (ChaosTokenFaceTarget token.face) $ sealChaosToken iid attrs token
        push unfocus
        doStep 1 msg
      Unrelenting1 <$> liftRunMessage msg attrs
    DoStep 1 (InvestigatorCommittedSkill iid sid) | sid == toId attrs -> do
      when (all ((`elem` [PlusOne, Zero, BlessToken, ElderSign]) . chaosTokenFace) attrs.sealed) do
        drawCards iid attrs 2
      pure s
    SkillTestEnds {} -> do
      for_ attrs.sealed unsealChaosToken
      pure s
    _ -> Unrelenting1 <$> liftRunMessage msg attrs
