module Arkham.Skill.Cards.Beloved (beloved) where

import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.I18n
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Beloved = Beloved SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beloved :: SkillCard Beloved
beloved = skillWith Beloved Cards.beloved (setMeta @Bool True)

instance RunMessage Beloved where
  runMessage msg s@(Beloved attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token -> do
      isBless <- token <=~> ChaosTokenFaceIs #bless
      getSkillTestId >>= \case
        Just sid
          | isBless
          , toResult attrs.meta -> do
              chooseOneM attrs.owner do
                cardI18n $ scope "beloved" $ labeled "removeForAutoSuccess" do
                  removeFromGame attrs
                  skillTestModifier sid attrs (ChaosTokenTarget token) ReturnBlessedToChaosBag
                  passSkillTest
                labeledI "doNotRemove" nothing
              pure $ Beloved $ attrs & setMeta False
        _ -> pure s
    _ -> Beloved <$> liftRunMessage msg attrs
