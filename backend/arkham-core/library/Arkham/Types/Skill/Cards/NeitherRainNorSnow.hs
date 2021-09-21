module Arkham.Types.Skill.Cards.NeitherRainNorSnow
  ( neitherRainNorSnow
  , NeitherRainNorSnow(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype NeitherRainNorSnow = NeitherRainNorSnow SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

neitherRainNorSnow :: SkillCard NeitherRainNorSnow
neitherRainNorSnow = skill NeitherRainNorSnow Cards.neitherRainNorSnow

instance SkillRunner env => RunMessage env NeitherRainNorSnow where
  runMessage msg s@(NeitherRainNorSnow attrs) = case msg of
    When (FailedSkillTest _ _ _ target _ _) | isTarget attrs target -> do
      removeAllMessagesMatching \case
        FailedSkillTest{} -> True
        _ -> False
      pure s
    _ -> NeitherRainNorSnow <$> runMessage msg attrs
