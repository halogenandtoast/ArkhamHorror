module Arkham.Homebrew.CircusExMortis.Stories.PathForward_178 (pathForward_178) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (RowEnd (..))
import Arkham.Homebrew.CircusExMortis.Stories.PathForward (pathForwardModifiers)
import Arkham.Story.Import.Lifted

newtype PathForward_178 = PathForward_178 StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Enters play facedown beside a row; act 1 flips the one beside your row faceup.
pathForward_178 :: StoryCard PathForward_178
pathForward_178 = storyWith PathForward_178 Cards.pathForward_178 (flippedL .~ True) & persistStory

-- This copy's column is "the leftmost location in this row".
instance HasModifiersFor PathForward_178 where
  getModifiersFor (PathForward_178 a) = pathForwardModifiers a (FromLeft 0)

instance RunMessage PathForward_178 where
  runMessage msg (PathForward_178 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> pure . PathForward_178 $ attrs & flippedL .~ False
    _ -> PathForward_178 <$> liftRunMessage msg attrs
