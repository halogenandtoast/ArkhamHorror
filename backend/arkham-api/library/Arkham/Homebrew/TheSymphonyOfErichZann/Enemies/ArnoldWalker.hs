module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.ArnoldWalker (arnoldWalker) where

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.SkillTest.Lifted (combinationSkillTest)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (instrumentInPlay)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Brass)
import Arkham.Matcher

newtype ArnoldWalker = ArnoldWalker EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arnoldWalker :: EnemyCard ArnoldWalker
arnoldWalker = enemy ArnoldWalker Cards.arnoldWalker

instance HasModifiersFor ArnoldWalker where
  -- "While there are no [[Brass]] treacheries in play, you cannot parley nor
  -- deal damage to Arnold Walker."
  getModifiersFor (ArnoldWalker a) = do
    unlocked <- instrumentInPlay Brass
    modifySelfWhen a (not unlocked) [CannotBeDamaged]

instance HasAbilities ArnoldWalker where
  getAbilities (ArnoldWalker a) =
    extend1 a
      $ restricted a 1 (OnSameLocation <> exists (withTrait Brass <> InPlayTreachery)) parleyAction_

instance RunMessage ArnoldWalker where
  runMessage msg e@(ArnoldWalker attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (DrawAdditionalChaosTokens 2 ResolveEach)
      combinationSkillTest
        sid
        iid
        (attrs.ability 1)
        attrs
        [#willpower, #intellect, #combat, #agility]
        (Fixed 5)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      card <- fetchCard Stories.trumpetersMuse
      readStory iid card Stories.trumpetersMuse
      pure e
    _ -> ArnoldWalker <$> liftRunMessage msg attrs
