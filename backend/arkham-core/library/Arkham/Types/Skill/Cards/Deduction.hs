{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Cards.Deduction where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype Deduction = Deduction Attrs
  deriving newtype (Show, ToJSON, FromJSON)

deduction :: InvestigatorId -> SkillId -> Deduction
deduction iid uuid = Deduction $ baseAttrs iid uuid "01039"

instance HasActions env Deduction where
  getActions i window (Deduction attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Deduction where
  runMessage msg s@(Deduction attrs@Attrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Investigate) _ (SkillTarget sid) _
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
