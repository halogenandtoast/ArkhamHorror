module Arkham.Homebrew.DarkMatter.Enemies.TheGreys (theGreys) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message (pattern BeginSkillTest)
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type

newtype TheGreys = TheGreys EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreys :: EnemyCard TheGreys
theGreys = enemy TheGreys Cards.theGreys

{- | "[action]: Parley. Test [willpower] + [intellect] (3). Reveal and resolve an
additional chaos token for this skill test. If you succeed, place 1 clue on the
current act (from the token bank)."
-}
instance HasAbilities TheGreys where
  getAbilities (TheGreys a) = extend1 a $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage TheGreys where
  runMessage msg e@(TheGreys attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let skills = [#willpower, #intellect]
      push
        $ BeginSkillTest
        $ ( buildSkillTest
              sid
              iid
              (attrs.ability 1)
              attrs
              (AndSkillTest skills)
              (AndSkillBaseValue skills)
              (SkillTestDifficulty $ Fixed 3)
          )
          { skillTestAction = Just #parley
          }
      skillTestModifier sid (attrs.ability 1) sid RevealAnotherChaosToken
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      act <- selectJust AnyAct
      placeClues (attrs.ability 1) act 1
      pure e
    _ -> TheGreys <$> liftRunMessage msg attrs
