{- | "If Griffin's evade value is a set number, resolve the evasion attempt,
card, or ability. Griffin gets +1 evade while resolving its effects.

Otherwise, if you are making an evasion attempt, this test automatically fails.
If you played a card or activated an ability, cancel each of its effects.
However, Griffin disengages from you and cannot engage you until the end of the
round. You cannot target Griffin with any attack, card or ability until the end
of the round.

In either case, shuffle Panicked Flight back into the Invisible Escape deck."

The "However" is the consolation for both halves of the failure, not a third
branch: you did not evade him, but he is off you for the round.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.PanickedFlight (panickedFlight) where

import Arkham.Helpers.Modifiers (ModifierType (EnemyEvade))
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Story.Import.Lifted

newtype PanickedFlight = PanickedFlight StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

panickedFlight :: StoryCard PanickedFlight
panickedFlight = story PanickedFlight Cards.panickedFlight

instance RunMessage PanickedFlight where
  runMessage msg s@(PanickedFlight attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- evasionOfGriffin attrs iid
      evadeIsSet <- griffinsEvadeIsASetNumber
      if evadeIsSet
        then griffinGetsWhileResolving attrs attempt (EnemyEvade 1)
        else do
          theEvasionFails attrs attempt
          griffinBreaksAwayFrom attrs iid
      shuffleBackIntoEscape attrs
      pure s
    _ -> PanickedFlight <$> liftRunMessage msg attrs
