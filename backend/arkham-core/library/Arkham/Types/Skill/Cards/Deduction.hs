module Arkham.Types.Skill.Cards.Deduction where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.SkillId
import Arkham.Types.Target


import qualified Arkham.Types.Action as Action
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype Deduction = Deduction SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction :: InvestigatorId -> SkillId -> Deduction
deduction iid uuid = Deduction $ baseAttrs iid uuid "01039"

instance HasModifiersFor env Deduction where
  getModifiersFor = noModifiersFor

instance HasActions env Deduction where
  getActions i window (Deduction attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Deduction where
  runMessage msg s@(Deduction attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Investigate) _ (SkillTarget sid) _ _
      | sid == skillId -> do
        lid <- getId @LocationId iid
        s <$ unshiftMessage
          (CreateEffect
            "01039"
            (Just $ EffectMetaTarget (LocationTarget lid))
            (toSource attrs)
            (InvestigatorTarget iid)
          )
    _ -> Deduction <$> runMessage msg attrs
