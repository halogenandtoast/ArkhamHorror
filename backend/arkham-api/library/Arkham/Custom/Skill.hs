-- | The runner behind a debug-authored custom skill. See "Arkham.Custom.Enemy".
module Arkham.Custom.Skill (CustomSkill (..), customSkill) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Custom.Ability (
  customAbilities,
  customModifiers,
  isCustomAbility,
  runCustomAbility,
  runCustomHandlers,
  pattern ZonedUseThisAbility,
 )
import Arkham.Skill.Import.Lifted

newtype CustomSkill = CustomSkill SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customSkill :: CardDef -> SkillCard CustomSkill
customSkill = skill CustomSkill

instance HasModifiersFor CustomSkill where
  getModifiersFor (CustomSkill a) = customModifiers a

instance HasAbilities CustomSkill where
  getAbilities (CustomSkill a) = customAbilities a

instance RunMessage CustomSkill where
  runMessage msg x@(CustomSkill attrs) = runQueueT $ case msg of
    ZonedUseThisAbility iid (isSource attrs -> True) idx ws | isCustomAbility attrs idx -> do
      runCustomAbility attrs iid idx ws
      pure x
    _ -> do
      runCustomHandlers attrs msg
      CustomSkill <$> liftRunMessage msg attrs
