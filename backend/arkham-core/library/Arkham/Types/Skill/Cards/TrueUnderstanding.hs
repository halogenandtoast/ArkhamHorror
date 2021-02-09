module Arkham.Types.Skill.Cards.TrueUnderstanding
  ( trueUnderstanding
  , TrueUnderstanding(..)
  )
where


import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype TrueUnderstanding = TrueUnderstanding SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueUnderstanding :: InvestigatorId -> SkillId -> TrueUnderstanding
trueUnderstanding iid uuid = TrueUnderstanding $ baseAttrs iid uuid "04153"

instance HasModifiersFor env TrueUnderstanding where
  getModifiersFor = noModifiersFor

instance HasActions env TrueUnderstanding where
  getActions iid window (TrueUnderstanding attrs) = getActions iid window attrs

-- Investigation is not an ability on the card so we need to pass
-- Nothing for the action type

instance SkillRunner env => RunMessage env TrueUnderstanding where
  runMessage msg s@(TrueUnderstanding attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == skillId -> do
      lid <- getId iid
      s <$ unshiftMessage (InvestigatorDiscoverClues iid lid 1 Nothing)
    _ -> TrueUnderstanding <$> runMessage msg attrs
