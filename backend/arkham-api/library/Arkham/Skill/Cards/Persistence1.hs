module Arkham.Skill.Cards.Persistence1 (persistence1, Persistence1 (..)) where

import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Strategy

newtype Persistence1 = Persistence1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

persistence1 :: SkillCard Persistence1
persistence1 = skill Persistence1 Cards.persistence1

instance HasModifiersFor Persistence1 where
  getModifiersFor (Persistence1 attrs) = case attrs.placement of
    StillInDiscard iid -> modified_ attrs iid [CanCommitToSkillTestsAsIfInHand (toCard attrs)]
    _ -> pure mempty

instance RunMessage Persistence1 where
  runMessage msg (Persistence1 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      attrs' <- liftRunMessage msg attrs
      case attrs.placement of
        StillInDiscard iid' | iid == iid' -> do
          pure $ Persistence1 $ attrs' & afterPlayL .~ ShuffleThisBackIntoDeck
        _ -> pure $ Persistence1 attrs'
    _ -> Persistence1 <$> liftRunMessage msg attrs
