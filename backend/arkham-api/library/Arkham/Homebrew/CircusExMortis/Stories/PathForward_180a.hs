{- | The second physical copy of Path Forward (:180). It needs its own card code
because 'Arkham.Id.StoryId' is the card code, so two copies of one column could not
otherwise sit beside two different rows at once; the def carries 'cdDuplicateOf' so the
card browser still lists the printed card once.
-}
module Arkham.Homebrew.CircusExMortis.Stories.PathForward_180a (pathForward_180a) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (RowEnd (..))
import Arkham.Homebrew.CircusExMortis.Stories.PathForward (pathForwardModifiers)
import Arkham.Story.Import.Lifted

newtype PathForward_180a = PathForward_180a StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Enters play facedown beside a row; act 1 flips the one beside your row faceup.
pathForward_180a :: StoryCard PathForward_180a
pathForward_180a =
  storyWith PathForward_180a Cards.pathForward_180a (flippedL .~ True) & persistStory

-- This copy's column is "the second location from the right".
instance HasModifiersFor PathForward_180a where
  getModifiersFor (PathForward_180a a) = pathForwardModifiers a (FromRight 1)

instance RunMessage PathForward_180a where
  runMessage msg (PathForward_180a attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> pure . PathForward_180a $ attrs & flippedL .~ False
    _ -> PathForward_180a <$> liftRunMessage msg attrs
