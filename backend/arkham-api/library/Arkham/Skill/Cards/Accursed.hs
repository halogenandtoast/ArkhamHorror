module Arkham.Skill.Cards.Accursed (accursed, Accursed (..)) where

import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag (getRemainingCurseTokens)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message (getChoiceAmount)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Accursed = Accursed SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accursed :: SkillCard Accursed
accursed = skill Accursed Cards.accursed

instance HasModifiersFor Accursed where
  getModifiersFor (Accursed a) =
    modifySelect a (ChaosTokenFaceIs #curse) [ChangeChaosTokenModifier (PositiveModifier 0)]

instance RunMessage Accursed where
  runMessage msg s@(Accursed attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      n <- getRemainingCurseTokens
      when (n > 0) do
        chooseAmount iid "Add up to 3 {curse} tokens to the chaos bag" "{curse} tokens" 0 (min 3 n) attrs
      Accursed <$> liftRunMessage msg attrs
    ResolveAmounts iid (getChoiceAmount "{curse} tokens" -> n) (isTarget attrs -> True) | n > 0 -> do
      addCurseTokens (Just iid) n
      pure s
    _ -> Accursed <$> liftRunMessage msg attrs
