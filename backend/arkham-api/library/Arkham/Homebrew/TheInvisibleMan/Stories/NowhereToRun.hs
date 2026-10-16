{- | "If you are making an evasion attempt, this test automatically fails. If
you played a card or activated an ability, cancel each of its effects.

If act 2a is in play, Griffin makes an immediate attack against you.

Shuffle Nowhere to Run back into the Invisible Escape deck."

Everywhere and Nowhere's twin in the Invisible Escape deck: nothing gets
through, and unlike the other two refusals Griffin does not break away
afterwards.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.NowhereToRun (nowhereToRun) where

import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Story.Import.Lifted

newtype NowhereToRun = NowhereToRun StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nowhereToRun :: StoryCard NowhereToRun
nowhereToRun = story NowhereToRun Cards.nowhereToRun

instance RunMessage NowhereToRun where
  runMessage msg s@(NowhereToRun attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- evasionOfGriffin attrs iid
      theEvasionFails attrs attempt
      whenM act2aInPlay $ griffinAttacks attrs iid
      shuffleBackIntoEscape attrs
      pure s
    _ -> NowhereToRun <$> liftRunMessage msg attrs
