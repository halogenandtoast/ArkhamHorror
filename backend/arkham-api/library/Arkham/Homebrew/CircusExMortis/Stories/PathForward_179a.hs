{- | The second physical copy of Path Forward (:179). It needs its own card code
because 'Arkham.Id.StoryId' is the card code, so two copies of one column could not
otherwise sit beside two different rows at once; the def carries 'cdDuplicateOf' so the
card browser still lists the printed card once.
-}
module Arkham.Homebrew.CircusExMortis.Stories.PathForward_179a (pathForward_179a) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (RowEnd (..))
import Arkham.Homebrew.CircusExMortis.Stories.PathForward (pathForwardModifiers)
import Arkham.Story.Import.Lifted

newtype PathForward_179a = PathForward_179a StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Enters play facedown beside a row; act 1 flips the one beside your row faceup.
pathForward_179a :: StoryCard PathForward_179a
pathForward_179a =
  storyWith PathForward_179a Cards.pathForward_179a (flippedL .~ True) & persistStory

-- This copy's column is "the second location from the left".
instance HasModifiersFor PathForward_179a where
  getModifiersFor (PathForward_179a a) = pathForwardModifiers a (FromLeft 1)

instance RunMessage PathForward_179a where
  runMessage msg (PathForward_179a attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> pure . PathForward_179a $ attrs & flippedL .~ False
    _ -> PathForward_179a <$> liftRunMessage msg attrs
