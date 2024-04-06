module Arkham.Skill.Cards.Fey1 (fey1, fey1Effect, Fey1 (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Prelude
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Fey1 = Fey1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fey1 :: SkillCard Fey1
fey1 = skill Fey1 Cards.fey1

instance RunMessage Fey1 where
  runMessage msg (Fey1 attrs) = case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      push $ createCardEffect Cards.fey1 Nothing attrs iid
      Fey1 <$> runMessage msg attrs
    _ -> Fey1 <$> runMessage msg attrs

newtype Fey1Effect = Fey1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fey1Effect :: EffectArgs -> Fey1Effect
fey1Effect = cardEffectWith Fey1Effect Cards.fey1 (setEffectMeta False)

instance RunMessage Fey1Effect where
  runMessage msg e@(Fey1Effect attrs) = case msg of
    RevealChaosToken _ _ token -> do
      pure $ if token.face == #curse then Fey1Effect $ setEffectMeta True attrs else e
    SkillTestEnded -> do
      let shouldReturn = toResult @Bool attrs.extra
      (card, owner) <- case attrs.source of
        SkillSource s -> (,) <$> field SkillCard s <*> field SkillOwner s
        _ -> error "Expected SkillSource"
      player <- getPlayer owner
      pushAll $ disable attrs
        : [ chooseOne
            player
            [ Label "Return Fey to hand" [ReturnToHand owner (CardIdTarget $ toCardId card)]
            , Label "Do not return" []
            ]
          | shouldReturn
          ]
      pure e
    _ -> Fey1Effect <$> runMessage msg attrs
