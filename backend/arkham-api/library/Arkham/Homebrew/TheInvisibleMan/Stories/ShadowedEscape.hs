{- | "If Griffin's evade value is a set number, resolve the evasion attempt,
card, or ability.

Otherwise, if you are making an evasion attempt, this test automatically fails.
If you played a card or activated an ability, cancel each of its effects.
However, Griffin disengages from you and cannot engage you until the end of the
round. You cannot target Griffin with any attack, card or ability until the end
of the round.

In either case, shuffle Shadowed Escape back into the Invisible Escape deck."

Panicked Flight without the +1 evade.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.ShadowedEscape (shadowedEscape) where

import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Story.Import.Lifted

newtype ShadowedEscape = ShadowedEscape StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowedEscape :: StoryCard ShadowedEscape
shadowedEscape = story ShadowedEscape Cards.shadowedEscape

instance RunMessage ShadowedEscape where
  runMessage msg s@(ShadowedEscape attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- evasionOfGriffin attrs iid
      unlessM griffinsEvadeIsASetNumber do
        theEvasionFails attrs attempt
        griffinBreaksAwayFrom attrs iid
      shuffleBackIntoEscape attrs
      pure s
    _ -> ShadowedEscape <$> liftRunMessage msg attrs
