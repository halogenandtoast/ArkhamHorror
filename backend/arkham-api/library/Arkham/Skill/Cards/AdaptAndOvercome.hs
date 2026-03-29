module Arkham.Skill.Cards.AdaptAndOvercome (adaptAndOvercome) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype AdaptAndOvercome = AdaptAndOvercome SkillAttrs
  deriving anyclass (IsSkill, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adaptAndOvercome :: SkillCard AdaptAndOvercome
adaptAndOvercome = skill AdaptAndOvercome Cards.adaptAndOvercome

instance HasModifiersFor AdaptAndOvercome where
  getModifiersFor (AdaptAndOvercome a) = runMaybeT_ do
    action <- MaybeT getSkillTestAction
    guard $ action `elem` [#fight, #evade]
    lift $ modifySelect a (InvestigatorWithId a.owner) [IgnoreAlert, IgnoreRetaliate]
