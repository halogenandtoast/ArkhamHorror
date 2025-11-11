module Arkham.Story.Cards.DeckOfPossibilities (deckOfPossibilities) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DeckOfPossibilities = DeckOfPossibilities StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deckOfPossibilities :: StoryCard DeckOfPossibilities
deckOfPossibilities = story DeckOfPossibilities Cards.deckOfPossibilities

instance RunMessage DeckOfPossibilities where
  runMessage msg s@(DeckOfPossibilities attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> DeckOfPossibilities <$> liftRunMessage msg attrs
