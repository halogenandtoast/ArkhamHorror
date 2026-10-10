module Arkham.Homebrew.AgesUnwound.Treacheries.RewrittenExistence (rewrittenExistence) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTestMatchingSkillIcons)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.SkillType
import Arkham.Treachery.Import.Lifted

newtype RewrittenExistence = RewrittenExistence TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rewrittenExistence :: TreacheryCard RewrittenExistence
rewrittenExistence = treachery RewrittenExistence Cards.rewrittenExistence

{- | "When you would test [combat], instead test [intellect], and vice versa.
When you would test [willpower], instead test [agility], and vice versa."

'getAlternateSkill' folds the @UseSkillInsteadOf@ modifiers, so publishing both
directions of a pair at once cancels them out. Each pair therefore emits only
the direction the test in progress actually needs (the Return to Canal
Saint-Martin shape).
-}
instance HasModifiersFor RewrittenExistence where
  getModifiersFor (RewrittenExistence a) = case a.placement of
    InThreatArea iid -> do
      kinds <- getSkillTestMatchingSkillIcons
      let
        pair x y
          | SkillIcon x `member` kinds = [UseSkillInsteadOf x y]
          | SkillIcon y `member` kinds = [UseSkillInsteadOf y x]
          | otherwise = []
      modified_ a iid $ pair #combat #intellect <> pair #willpower #agility
    _ -> pure mempty

{- | "Forced - At the end of the round: Test any skill (3). If you succeed,
discard Rewritten Existence."
-}
instance HasAbilities RewrittenExistence where
  getAbilities (RewrittenExistence a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) $ forced $ RoundEnds #when]

instance RunMessage RewrittenExistence where
  runMessage msg t@(RewrittenExistence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ allSkills \s -> skillLabeled s $ beginSkillTest sid iid (attrs.ability 1) iid s (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> RewrittenExistence <$> liftRunMessage msg attrs
