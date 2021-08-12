module Arkham.Types.Skill.Cards.Deduction2 where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype Deduction2 = Deduction2 SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction2 :: SkillCard Deduction2
deduction2 = skill Deduction2 Cards.deduction2

instance HasModifiersFor env Deduction2
instance HasActions Deduction2

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
