module Arkham.Types.Skill.Cards.Guts where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype Guts = Guts SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guts :: SkillCard Guts
guts = skill Guts Cards.guts

instance HasModifiersFor env Guts where
  getModifiersFor = noModifiersFor

instance HasActions env Guts where
  getActions i window (Guts attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Guts where
  runMessage msg s@(Guts attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ unshiftMessage (DrawCards skillOwner 1 False)
    _ -> Guts <$> runMessage msg attrs
