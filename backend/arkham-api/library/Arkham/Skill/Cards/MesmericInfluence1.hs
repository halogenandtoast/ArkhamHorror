module Arkham.Skill.Cards.MesmericInfluence1 (mesmericInfluence1, MesmericInfluence1 (..)) where

import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype MesmericInfluence1 = MesmericInfluence1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmericInfluence1 :: SkillCard MesmericInfluence1
mesmericInfluence1 = skill MesmericInfluence1 Cards.mesmericInfluence1

instance HasModifiersFor MesmericInfluence1 where
  getModifiersFor (InvestigatorTarget iid) (MesmericInfluence1 a) = maybeModified a do
    iid' <- MaybeT getSkillTestInvestigator
    guard $ iid == iid'
    pure [MayIgnoreLocationEffectsAndKeywords]
  getModifiersFor _ _ = pure []

instance RunMessage MesmericInfluence1 where
  runMessage msg (MesmericInfluence1 attrs) = runQueueT $ case msg of
    _ -> MesmericInfluence1 <$> liftRunMessage msg attrs
