module Arkham.Skill.Cards.BruteForce1 (
  bruteForce1,
  BruteForce1 (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Constants
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype BruteForce1 = BruteForce1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bruteForce1 :: SkillCard BruteForce1
bruteForce1 = skill BruteForce1 Cards.bruteForce1

instance HasModifiersFor BruteForce1 where
  getModifiersFor target (BruteForce1 a) | a `is` target = do
    mAction <- getSkillTestAction
    mSource <- getSkillTestSource
    case (mAction, mSource) of
      (Just Action.Fight, Just (AbilitySource (EnemySource _) AbilityAttack)) -> pure $ toModifiers a [AddSkillIcons [#combat, #combat]]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage BruteForce1 where
  runMessage msg s@(BruteForce1 attrs) = case msg of
    PassedSkillTest {} -> do
      mAction <- getSkillTestAction
      mSource <- getSkillTestSource
      mInvestigator <- getSkillTestInvestigator
      case (mAction, mSource, mInvestigator) of
        (Just Action.Fight, Just (AbilitySource (EnemySource _) AbilityAttack), Just iid) -> push $ skillTestModifier (toSource attrs) iid (DamageDealt 2)
        _ -> pure ()
      pure s
    _ -> BruteForce1 <$> runMessage msg attrs
