module Arkham.Skill.Cards.NeitherRainNorSnow
  ( neitherRainNorSnow
  , NeitherRainNorSnow(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Helpers.Window
import Arkham.Message
import Arkham.Skill.Runner
import Arkham.Timing qualified as Timing
import Arkham.Window qualified as Window
import Arkham.Window (Window(..))

newtype NeitherRainNorSnow = NeitherRainNorSnow SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

neitherRainNorSnow :: SkillCard NeitherRainNorSnow
neitherRainNorSnow = skill NeitherRainNorSnow Cards.neitherRainNorSnow

instance RunMessage NeitherRainNorSnow where
  runMessage msg s@(NeitherRainNorSnow attrs) = case msg of
    When (FailedSkillTest _ _ _ target _ _) | isTarget attrs target -> do
      ignoreWindow <- checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      removeAllMessagesMatching \case
        FailedSkillTest{} -> True
        _ -> False
      push ignoreWindow
      pure s
    _ -> NeitherRainNorSnow <$> runMessage msg attrs
