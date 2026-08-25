module Arkham.Homebrew.CircusExMortis.Skills.InvocationOfDiana (invocationOfDiana) where

import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, withSkillTest)
import Arkham.Homebrew.CircusExMortis.CardDefs.Skills qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Import.Lifted

newtype InvocationOfDiana = InvocationOfDiana SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

invocationOfDiana :: SkillCard InvocationOfDiana
invocationOfDiana = skill InvocationOfDiana Cards.invocationOfDiana

{- | "Cancel each moon token revealed during this test." Modeled the way Defiance
models cancelling a face for a whole test: the token resolves no effects, so it
is neither sealed nor replaced by another draw.
-}
instance HasModifiersFor InvocationOfDiana where
  getModifiersFor (InvocationOfDiana attrs) = withSkillTest \_ ->
    modified_ attrs (ChaosTokenFaceTarget MoonToken) [IgnoreChaosTokenEffects]

instance RunMessage InvocationOfDiana where
  runMessage msg s@(InvocationOfDiana attrs) = runQueueT $ case msg of
    ResolveChaosToken _ MoonToken _ -> do
      cancelledOrIgnoredCardOrGameEffect attrs
      pure s
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      skillTestCardOption attrs $ doStep 1 msg
      pure s
    DoStep 1 (PassedSkillTest _ _ _ (isTarget attrs -> True) _ _) -> do
      getSkillTestInvestigator >>= traverse_ \iid -> do
        tokens <- getSealedMoonTokensControlledBy iid
        chooseOneM iid $ campaignI18n $ scope "invocationOfDiana" do
          unscoped $ countVar 2 $ labeled' "drawCards" $ drawCards iid attrs 2
          when (notNull tokens) $ labeled' "releaseTokens" $ doStep 2 msg
          unscoped $ labeled' "doNothing" nothing
      pure s
    DoStep 2 (DoStep 1 (PassedSkillTest _ _ _ (isTarget attrs -> True) _ _)) -> do
      getSkillTestInvestigator >>= traverse_ \iid ->
        chooseReleaseTokens iid 2 =<< getSealedMoonTokensControlledBy iid
      pure s
    _ -> InvocationOfDiana <$> liftRunMessage msg attrs
