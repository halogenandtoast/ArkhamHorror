module Arkham.Skill.Cards.GrimResolve (grimResolve) where

import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype GrimResolve = GrimResolve SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grimResolve :: SkillCard GrimResolve
grimResolve = skill GrimResolve Cards.grimResolve

instance RunMessage GrimResolve where
  runMessage msg s@(GrimResolve attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      afterSkillTest iid "Grim Resolve" $ forTarget attrs msg
      pure s
    FailedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      afterSkillTest iid "Grim Resolve" $ forTarget attrs msg
      pure s
    ForTarget (isTarget attrs -> True) _ -> do
      under <- field InvestigatorCardsUnderneath attrs.owner
      hand <- filterCards NonWeakness <$> field InvestigatorHand attrs.owner

      when (notNull under && notNull hand) do
        sendShowUnder attrs.owner
        chooseOneM attrs.owner do
          labeled "Done swapping" nothing
          targets under \card -> do
            chooseOneM attrs.owner do
              questionLabeled "Choose hand card to swap with"
              targets hand \handCard -> do
                addToHand attrs.owner (only card)
                placeUnderneath attrs.owner [handCard]
                push msg
          targets hand \handCard -> do
            chooseOneM attrs.owner do
              questionLabeled "Choose card underneath to swap with"
              targets under \card -> do
                addToHand attrs.owner (only card)
                placeUnderneath attrs.owner [handCard]
                push msg
      pure s
    _ -> GrimResolve <$> liftRunMessage msg attrs
