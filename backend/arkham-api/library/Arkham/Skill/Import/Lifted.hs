module Arkham.Skill.Import.Lifted (module X, module Arkham.Skill.Import.Lifted) where

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.Helpers.Modifiers as X (getModifiers)
import Arkham.Id as X
import Arkham.Message as X (
  Message (..),
  toMessage,
  pattern CancelRevelation,
  pattern FailedThisSkillTest,
  pattern PassedThisSkillTest,
  pattern PassedThisSkillTestBy,
  pattern PlaceClues,
  pattern PlayThisEvent,
  pattern RemoveDoom,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.Skill.Runner as X (
  IsSkill,
  SkillAttrs (..),
  SkillCard,
  additionalCostL,
  afterPlayL,
  is,
  push,
  pushAll,
  pushAllM,
  pushM,
  pushWhen,
  pushWhenM,
  setMeta,
  skill,
  skillWith,
 )
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Helpers.Modifiers (ModifierType (Semaphore), modifySelfWhen, semaphore)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier (ModifierType (AddSkillIcons))
import Arkham.SkillType (SkillIcon)

addSkillIconsWhen :: HasModifiersM m => SkillAttrs -> Bool -> [SkillIcon] -> m ()
addSkillIconsWhen _attrs _cond [] = pure ()
addSkillIconsWhen attrs cond icons = modifySelfWhen attrs.cardId cond [AddSkillIcons icons]

addSkillIcons :: HasModifiersM m => SkillAttrs -> [SkillIcon] -> m ()
addSkillIcons attrs = addSkillIconsWhen attrs True

removeSkill :: (ToId a SkillId, ReverseQueue m) => a -> m ()
removeSkill (asId -> skillId) = push $ RemoveSkill skillId

onlyOnceDuringSkillTest :: ReverseQueue m => SkillAttrs -> (SkillTestId -> m ()) -> m ()
onlyOnceDuringSkillTest attrs body = semaphore attrs do
  withSkillTest \sid -> do
    skillTestModifier sid attrs attrs Semaphore
    body sid
