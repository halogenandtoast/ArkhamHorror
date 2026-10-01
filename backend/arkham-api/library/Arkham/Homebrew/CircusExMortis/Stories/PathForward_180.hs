module Arkham.Homebrew.CircusExMortis.Stories.PathForward_180 (pathForward_180) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (RowEnd (..))
import Arkham.Homebrew.CircusExMortis.Stories.PathForward (pathForwardModifiers)
import Arkham.Story.Import.Lifted

newtype PathForward_180 = PathForward_180 StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Enters play facedown beside a row; act 1 flips the one beside your row faceup.
pathForward_180 :: StoryCard PathForward_180
pathForward_180 = storyWith PathForward_180 Cards.pathForward_180 (flippedL .~ True) & persistStory

-- This copy's column is "the second location from the right in this row".
instance HasModifiersFor PathForward_180 where
  getModifiersFor (PathForward_180 a) = pathForwardModifiers a (FromRight 1)

instance RunMessage PathForward_180 where
  runMessage msg (PathForward_180 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> pure . PathForward_180 $ attrs & flippedL .~ False
    _ -> PathForward_180 <$> liftRunMessage msg attrs
