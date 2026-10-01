{- | The second physical copy of Path Forward (:181). It needs its own card code
because 'Arkham.Id.StoryId' is the card code, so two copies of one column could not
otherwise sit beside two different rows at once; the def carries 'cdDuplicateOf' so the
card browser still lists the printed card once.
-}
module Arkham.Homebrew.CircusExMortis.Stories.PathForward_181a (pathForward_181a) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (RowEnd (..))
import Arkham.Homebrew.CircusExMortis.Stories.PathForward (pathForwardModifiers)
import Arkham.Story.Import.Lifted

newtype PathForward_181a = PathForward_181a StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Enters play facedown beside a row; act 1 flips the one beside your row faceup.
pathForward_181a :: StoryCard PathForward_181a
pathForward_181a =
  storyWith PathForward_181a Cards.pathForward_181a (flippedL .~ True) & persistStory

-- This copy's column is "the rightmost location in this row".
instance HasModifiersFor PathForward_181a where
  getModifiersFor (PathForward_181a a) = pathForwardModifiers a (FromRight 0)

instance RunMessage PathForward_181a where
  runMessage msg (PathForward_181a attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> pure . PathForward_181a $ attrs & flippedL .~ False
    _ -> PathForward_181a <$> liftRunMessage msg attrs
