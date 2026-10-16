{- | "Resolve the attack, card, or ability. If this effect damages Griffin, deal
1 additional damage to him.

Shuffle Subtle Signs back into the Invisible Combat deck."

Set aside at setup; act 2d is what shuffles it into the Invisible Combat deck.
"If this effect damages Griffin" needs no test of its own -- a @DamageTaken@
modifier on him only ever adds to damage that is actually dealt.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.SubtleSigns (subtleSigns) where

import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Story.Import.Lifted

newtype SubtleSigns = SubtleSigns StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

subtleSigns :: StoryCard SubtleSigns
subtleSigns = story SubtleSigns Cards.subtleSigns

instance RunMessage SubtleSigns where
  runMessage msg s@(SubtleSigns attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- attackOnGriffin attrs iid
      additionalDamageToGriffin attrs attempt 1
      shuffleBackIntoCombat attrs
      pure s
    _ -> SubtleSigns <$> liftRunMessage msg attrs
