module Arkham.Skill.Cards.Fey1 (fey1, fey1Effect, Fey1 (..)) where

import Arkham.Effect.Import
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Skill.Types (Field (SkillCard, SkillOwner))

newtype Fey1 = Fey1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fey1 :: SkillCard Fey1
fey1 = skill Fey1 Cards.fey1

instance RunMessage Fey1 where
  runMessage msg (Fey1 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      createCardEffect Cards.fey1 Nothing attrs iid
      Fey1 <$> liftRunMessage msg attrs
    _ -> Fey1 <$> liftRunMessage msg attrs

newtype Fey1Effect = Fey1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fey1Effect :: EffectArgs -> Fey1Effect
fey1Effect = cardEffectWith Fey1Effect Cards.fey1 (setEffectMeta False)

instance RunMessage Fey1Effect where
  runMessage msg e@(Fey1Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token -> do
      pure $ if token.face == #curse then Fey1Effect $ setEffectMeta True attrs else e
    SkillTestEnded _ -> do
      let shouldReturn = toResult @Bool attrs.extra
      (card, owner) <- case attrs.source of
        SkillSource s -> (,) <$> field SkillCard s <*> field SkillOwner s
        _ -> error "Expected SkillSource"
      disable attrs
      when shouldReturn do
        chooseOneM owner do
          labeled "Return Fey to hand" $ returnToHand owner card
          labeled "Do not return" nothing
      pure e
    _ -> Fey1Effect <$> liftRunMessage msg attrs
