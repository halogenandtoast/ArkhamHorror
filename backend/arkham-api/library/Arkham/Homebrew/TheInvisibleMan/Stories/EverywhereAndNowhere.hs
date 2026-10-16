{- | "If you are attacking, this attack automatically fails. If you played a
card or activated an ability, cancel each of its effects.

If act 2a is in play, Griffin makes an immediate attack against you.

Shuffle Everywhere and Nowhere back into the Invisible Combat deck."

The one card in the Invisible Combat deck that never lets anything through,
whether or not Griffin's fight value has been pinned down.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.EverywhereAndNowhere (
  everywhereAndNowhere,
) where

import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Story.Import.Lifted

newtype EverywhereAndNowhere = EverywhereAndNowhere StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everywhereAndNowhere :: StoryCard EverywhereAndNowhere
everywhereAndNowhere = story EverywhereAndNowhere Cards.everywhereAndNowhere

instance RunMessage EverywhereAndNowhere where
  runMessage msg s@(EverywhereAndNowhere attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- attackOnGriffin attrs iid
      theAttackFails attrs attempt
      whenM act2aInPlay $ griffinAttacks attrs iid
      shuffleBackIntoCombat attrs
      pure s
    _ -> EverywhereAndNowhere <$> liftRunMessage msg attrs
