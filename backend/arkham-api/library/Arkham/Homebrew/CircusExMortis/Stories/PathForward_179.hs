module Arkham.Homebrew.CircusExMortis.Stories.PathForward_179 (pathForward_179) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (RowEnd (..))
import Arkham.Homebrew.CircusExMortis.Stories.PathForward (pathForwardModifiers)
import Arkham.Story.Import.Lifted

newtype PathForward_179 = PathForward_179 StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Enters play facedown beside a row; act 1 flips the one beside your row faceup.
pathForward_179 :: StoryCard PathForward_179
pathForward_179 = storyWith PathForward_179 Cards.pathForward_179 (flippedL .~ True) & persistStory

-- This copy's column is "the second location from the left in this row".
instance HasModifiersFor PathForward_179 where
  getModifiersFor (PathForward_179 a) = pathForwardModifiers a (FromLeft 1)

instance RunMessage PathForward_179 where
  runMessage msg (PathForward_179 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> pure . PathForward_179 $ attrs & flippedL .~ False
    _ -> PathForward_179 <$> liftRunMessage msg attrs
