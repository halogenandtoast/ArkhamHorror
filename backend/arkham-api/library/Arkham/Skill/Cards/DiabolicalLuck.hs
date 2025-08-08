module Arkham.Skill.Cards.DiabolicalLuck (diabolicalLuck) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (RevealChaosToken)

newtype DiabolicalLuck = DiabolicalLuck SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diabolicalLuck :: SkillCard DiabolicalLuck
diabolicalLuck = skill DiabolicalLuck Cards.diabolicalLuck

instance HasAbilities DiabolicalLuck where
  getAbilities (DiabolicalLuck x) =
    [ displayAsAction
        $ restricted x 1 InYourHand
        $ ConstantReaction "Commit Diabolical Luck from your hand" (RevealChaosToken #when You #curse) Free
    ]

instance RunMessage DiabolicalLuck where
  runMessage msg s@(DiabolicalLuck attrs) = runQueueT $ case msg of
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) attrs.cardId (AddSkillIcons [#wild, #wild, #wild])
        push $ CommitCard iid (toCard attrs)
      pure s
    _ -> DiabolicalLuck <$> liftRunMessage msg attrs
