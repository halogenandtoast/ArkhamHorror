{- | "Resolve the attack, card, or ability. If this effect damages Griffin, deal
2 additional damage to him.

Shuffle Got His Number back into the Invisible Combat deck."

Set aside at setup, like Subtle Signs; act 2d is what shuffles it into the
Invisible Combat deck.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.GotHisNumber (gotHisNumber) where

import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Story.Import.Lifted

newtype GotHisNumber = GotHisNumber StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gotHisNumber :: StoryCard GotHisNumber
gotHisNumber = story GotHisNumber Cards.gotHisNumber

instance RunMessage GotHisNumber where
  runMessage msg s@(GotHisNumber attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- attackOnGriffin attrs iid
      additionalDamageToGriffin attrs attempt 2
      shuffleBackIntoCombat attrs
      pure s
    _ -> GotHisNumber <$> liftRunMessage msg attrs
