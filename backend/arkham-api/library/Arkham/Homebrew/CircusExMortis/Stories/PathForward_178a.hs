{- | The second physical copy of Path Forward (:178). It needs its own card code
because 'Arkham.Id.StoryId' is the card code, so two copies of one column could not
otherwise sit beside two different rows at once. The suffix is @a@, not @b@: @b@ is
already how 'Arkham.Card.CardCode.flippedCardCode' names a card's back face. The def
carries 'cdDuplicateOf' so the card browser still lists the printed card once.
-}
module Arkham.Homebrew.CircusExMortis.Stories.PathForward_178a (pathForward_178a) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (RowEnd (..))
import Arkham.Homebrew.CircusExMortis.Stories.PathForward (pathForwardModifiers)
import Arkham.Story.Import.Lifted

newtype PathForward_178a = PathForward_178a StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Enters play facedown beside a row; act 1 flips the one beside your row faceup.
pathForward_178a :: StoryCard PathForward_178a
pathForward_178a =
  storyWith PathForward_178a Cards.pathForward_178a (flippedL .~ True) & persistStory

-- This copy's column is "the leftmost location in this row".
instance HasModifiersFor PathForward_178a where
  getModifiersFor (PathForward_178a a) = pathForwardModifiers a (FromLeft 0)

instance RunMessage PathForward_178a where
  runMessage msg (PathForward_178a attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> pure . PathForward_178a $ attrs & flippedL .~ False
    _ -> PathForward_178a <$> liftRunMessage msg attrs
