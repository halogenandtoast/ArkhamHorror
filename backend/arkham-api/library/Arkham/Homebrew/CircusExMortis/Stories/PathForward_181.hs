module Arkham.Homebrew.CircusExMortis.Stories.PathForward_181 (pathForward_181) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (RowEnd (..))
import Arkham.Homebrew.CircusExMortis.Stories.PathForward (pathForwardModifiers)
import Arkham.Story.Import.Lifted

newtype PathForward_181 = PathForward_181 StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Enters play facedown beside a row; act 1 flips the one beside your row faceup.
pathForward_181 :: StoryCard PathForward_181
pathForward_181 = storyWith PathForward_181 Cards.pathForward_181 (flippedL .~ True) & persistStory

-- This copy's column is "the rightmost location in this row".
instance HasModifiersFor PathForward_181 where
  getModifiersFor (PathForward_181 a) = pathForwardModifiers a (FromRight 0)

instance RunMessage PathForward_181 where
  runMessage msg (PathForward_181 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> pure . PathForward_181 $ attrs & flippedL .~ False
    _ -> PathForward_181 <$> liftRunMessage msg attrs
