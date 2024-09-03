module Arkham.Skill.Cards.Deduction2 where

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Discover
import Arkham.Effect.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Deduction2 = Deduction2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction2 :: SkillCard Deduction2
deduction2 = skill Deduction2 Cards.deduction2

instance RunMessage Deduction2 where
  runMessage msg s@(Deduction2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Investigate) _ (SkillTarget sid) _ n | sid == skillId -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> do
        let effectMessage = createCardEffect Cards.deduction2 (Just $ EffectMetaTarget (toTarget lid)) attrs iid
        pushAll $ replicate (if n >= 2 then 2 else 1) effectMessage
      pure s
    _ -> Deduction2 <$> runMessage msg attrs

newtype Deduction2Effect = Deduction2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction2Effect :: EffectArgs -> Deduction2Effect
deduction2Effect = cardEffect Deduction2Effect Cards.deduction2

instance RunMessage Deduction2Effect where
  runMessage msg e@(Deduction2Effect attrs@EffectAttrs {..}) = case msg of
    Successful (Action.Investigate, _) iid _ (LocationTarget lid) _ ->
      case effectMetadata of
        Just (EffectMetaTarget (LocationTarget lid')) | lid == lid' -> do
          push $ DiscoverClues iid $ viaInvestigate $ discover lid (toSource attrs) 1
          pure e
        _ -> pure e
    SkillTestEnds _ _ _ -> e <$ push (DisableEffect effectId)
    _ -> Deduction2Effect <$> runMessage msg attrs
