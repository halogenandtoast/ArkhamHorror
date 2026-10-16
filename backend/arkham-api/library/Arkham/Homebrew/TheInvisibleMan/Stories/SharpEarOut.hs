{- | "Resolve the evasion attempt, card, or ability. If this effect evades
Griffin, deal 1 damage to him.

Shuffle Sharp Ear Out back into the Invisible Escape deck."

Set aside at setup; act 2f is what shuffles it into the Invisible Escape deck.

An evasion attempt only evades him if it passes, so the damage hangs off the
test's success. A card or ability that evades him automatically has already
been let through by the time this resolves, so the damage is simply dealt.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.SharpEarOut (sharpEarOut) where

import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype SharpEarOut = SharpEarOut StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sharpEarOut :: StoryCard SharpEarOut
sharpEarOut = story SharpEarOut Cards.sharpEarOut

instance RunMessage SharpEarOut where
  runMessage msg s@(SharpEarOut attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- evasionOfGriffin attrs iid
      withGriffin \eid -> case attempt of
        AttemptDeclared sid ->
          onSucceedByEffect sid (atLeast 0) attrs iid
            $ nonAttackEnemyDamage (Just iid) attrs 1 eid
        AutomaticEffect -> nonAttackEnemyDamage (Just iid) attrs 1 eid
      shuffleBackIntoEscape attrs
      pure s
    _ -> SharpEarOut <$> liftRunMessage msg attrs
