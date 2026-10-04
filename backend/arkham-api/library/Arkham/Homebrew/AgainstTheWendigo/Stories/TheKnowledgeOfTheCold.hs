module Arkham.Homebrew.AgainstTheWendigo.Stories.TheKnowledgeOfTheCold (
  theKnowledgeOfTheCold,
) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Story.Import.Lifted

newtype TheKnowledgeOfTheCold = TheKnowledgeOfTheCold StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKnowledgeOfTheCold :: StoryCard TheKnowledgeOfTheCold
theKnowledgeOfTheCold = story TheKnowledgeOfTheCold Cards.theKnowledgeOfTheCold

{- | The second part replaces the Temple of Ithaqua with Ithaqua itself: "Flip
this card, put it into play in place of Temple of Ithaqua... Put Temple of
Ithaqua in the victory display with the horrors that were on it."
-}
instance RunMessage TheKnowledgeOfTheCold where
  runMessage msg s@(TheKnowledgeOfTheCold attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.templeOfIthaqua) \temple -> do
        addToVictory iid temple
        push $ RemoveLocation temple
      -- Ithaqua's abilities are all on its revealed side, so it arrives face up.
      reveal =<< placeLocationCard Locations.ithaqua
      record YouAreTheCustodianOfIthaquasKnowledge
      removeStory attrs
      pure s
    _ -> TheKnowledgeOfTheCold <$> liftRunMessage msg attrs
