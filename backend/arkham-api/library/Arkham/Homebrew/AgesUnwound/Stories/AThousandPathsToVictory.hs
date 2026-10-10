module Arkham.Homebrew.AgesUnwound.Stories.AThousandPathsToVictory (
  aThousandPathsToVictory,
) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Story.Import.Lifted

newtype AThousandPathsToVictory = AThousandPathsToVictory StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The reverse of the @:ages-unwound:195@ printing of /Today, a Thousand Times/.
Act 4's __Forced__ flips a location from beneath the agenda deck and resolves its
text; for this printing that text is a story.
-}
aThousandPathsToVictory :: StoryCard AThousandPathsToVictory
aThousandPathsToVictory = story AThousandPathsToVictory Cards.aThousandPathsToVictory

{- | "Place 1 doom on the current agenda. Remove Today, a Thousand Times from the
game."
-}
instance RunMessage AThousandPathsToVictory where
  runMessage msg s@(AThousandPathsToVictory attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      placeDoomOnAgenda 1
      for_ (storyOtherSide attrs) removeFromGame
      removeFromGame attrs
      pure s
    _ -> AThousandPathsToVictory <$> liftRunMessage msg attrs
