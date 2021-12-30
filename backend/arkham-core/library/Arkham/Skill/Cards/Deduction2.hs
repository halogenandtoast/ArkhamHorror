module Arkham.Skill.Cards.Deduction2 where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Id
import Arkham.Message
import Arkham.Skill.Attrs
import Arkham.Skill.Runner
import Arkham.Target

newtype Deduction2 = Deduction2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction2 :: SkillCard Deduction2
deduction2 = skill Deduction2 Cards.deduction2

instance (SkillRunner env) => RunMessage env Deduction2 where
  runMessage msg s@(Deduction2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Investigate) _ (SkillTarget sid) _ n
      | sid == skillId -> do
        lid <- getId @LocationId iid

        let
          effectMessage = CreateEffect
            (toCardCode attrs)
            (Just $ EffectMetaTarget (LocationTarget lid))
            (toSource attrs)
            (InvestigatorTarget iid)
        if n >= 2
          then s <$ pushAll [effectMessage, effectMessage]
          else s <$ push effectMessage
    _ -> Deduction2 <$> runMessage msg attrs
