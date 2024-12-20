module Arkham.Story.Cards.DeckOfTheTheodosia (deckOfTheTheodosia) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DeckOfTheTheodosia = DeckOfTheTheodosia StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deckOfTheTheodosia :: StoryCard DeckOfTheTheodosia
deckOfTheTheodosia = story DeckOfTheTheodosia Cards.deckOfTheTheodosia

instance RunMessage DeckOfTheTheodosia where
  runMessage msg s@(DeckOfTheTheodosia attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mCoastalWaters <- getSetAsideCardMaybe Locations.coastalWaters
      mHedgeMaze <- getSetAsideCardMaybe Locations.hedgeMaze
      mStandingStones <- getSetAsideCardMaybe Locations.standingStones

      chooseOneM iid do
        for_ mCoastalWaters
          $ labeled "Put the set-aside Coastal Waters location into play."
          . placeLocation_
        for_ mHedgeMaze
          $ labeled "Put the set-aside Hedge Maze location into play."
          . placeLocation_
        for_ mStandingStones
          $ labeled "Put the set-aside Standing Stones location into play."
          . placeLocation_
      pure s
    _ -> DeckOfTheTheodosia <$> liftRunMessage msg attrs
