module Arkham.Skill.Cards.Deduction2 where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
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
    PassedSkillTest iid (Just Action.Investigate) _ (SkillTarget sid) _ n
      | sid == skillId -> do
          mlid <- field InvestigatorLocation iid
          case mlid of
            Nothing -> pure ()
            Just lid -> do
              let
                effectMessage =
                  CreateEffect
                    (toCardCode attrs)
                    (Just $ EffectMetaTarget (LocationTarget lid))
                    (toSource attrs)
                    (InvestigatorTarget iid)
              if n >= 2
                then pushAll [effectMessage, effectMessage]
                else push effectMessage
          pure s
    _ -> Deduction2 <$> runMessage msg attrs
