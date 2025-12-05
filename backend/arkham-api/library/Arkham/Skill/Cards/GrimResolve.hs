module Arkham.Skill.Cards.GrimResolve (grimResolve, grimResolveEffect) where

import Arkham.Card
import Arkham.Effect.Import
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
      createCardEffect Cards.grimResolve Nothing attrs iid
      pure s
    FailedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      createCardEffect Cards.grimResolve Nothing attrs iid
      pure s
    _ -> GrimResolve <$> liftRunMessage msg attrs

newtype GrimResolveEffect = GrimResolveEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grimResolveEffect :: EffectArgs -> GrimResolveEffect
grimResolveEffect = cardEffect GrimResolveEffect Cards.grimResolve

instance RunMessage GrimResolveEffect where
  runMessage msg e@(GrimResolveEffect attrs) = runQueueT $ case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == attrs.id -> do
      afterSkillTest iid "Grim Resolve" $ forTarget attrs msg
      pure e
    ForTarget (isTarget attrs -> True) _ -> do
      for_ attrs.target.investigator \iid -> do
        under <- field InvestigatorCardsUnderneath iid
        hand <- filterCards NonWeakness <$> field InvestigatorHand iid

        when (notNull under && notNull hand) do
          sendShowUnder iid
          chooseOneM iid do
            labeled "Done swapping" nothing
            targets under \card -> do
              chooseOneM iid do
                questionLabeled "Choose hand card to swap with"
                targets hand \handCard -> do
                  addToHand iid (only card)
                  placeUnderneath iid [handCard]
                  push msg
            targets hand \handCard -> do
              chooseOneM iid do
                questionLabeled "Choose card underneath to swap with"
                targets under \card -> do
                  addToHand iid (only card)
                  placeUnderneath iid [handCard]
                  push msg
      disableReturn e
    _ -> GrimResolveEffect <$> liftRunMessage msg attrs
