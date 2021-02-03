module Arkham.Types.Skill.Cards.Opportunist where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Target

newtype Opportunist = Opportunist SkillAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

opportunist :: InvestigatorId -> SkillId -> Opportunist
opportunist iid uuid = Opportunist $ baseAttrs iid uuid "01053"

instance HasModifiersFor env Opportunist where
  getModifiersFor = noModifiersFor

instance HasActions env Opportunist where
  getActions i window (Opportunist attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Opportunist where
  runMessage msg s@(Opportunist attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ n | sid == skillId && n >= 3 ->
      s <$ unshiftMessage (ReturnToHand iid (SkillTarget skillId))
    _ -> Opportunist <$> runMessage msg attrs
