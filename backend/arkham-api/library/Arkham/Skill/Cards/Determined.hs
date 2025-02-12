module Arkham.Skill.Cards.Determined (determined) where

import Arkham.Event.Types (Field (..))
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Strategy

newtype Determined = Determined SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

determined :: SkillCard Determined
determined = skill Determined Cards.determined

instance RunMessage Determined where
  runMessage msg s@(Determined attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      mEventPlayedBy <- runMaybeT do
        EventSource eid <- MaybeT getSkillTestSource
        lift $ field EventController eid

      if attrs.owner `elem` mEventPlayedBy
        then pure $ Determined $ attrs & afterPlayL .~ ReturnThisToHand
        else pure s
    _ -> Determined <$> liftRunMessage msg attrs
