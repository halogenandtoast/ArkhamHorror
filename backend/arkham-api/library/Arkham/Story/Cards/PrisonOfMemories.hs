module Arkham.Story.Cards.PrisonOfMemories (prisonOfMemories) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype PrisonOfMemories = PrisonOfMemories StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prisonOfMemories :: StoryCard PrisonOfMemories
prisonOfMemories = story PrisonOfMemories Cards.prisonOfMemories

instance RunMessage PrisonOfMemories where
  runMessage msg s@(PrisonOfMemories attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mBaseCamp <- getSetAsideCardMaybe Locations.baseCamp
      mDeckOfTheTheodosia <- getSetAsideCardMaybe Locations.deckOfTheTheodosia
      mUniversityOfHalls <- getSetAsideCardMaybe Locations.universityHalls

      chooseOneM iid do
        for_ mBaseCamp $ labeled "Put the set-aside Base Camp location into play." . placeLocation_
        for_ mDeckOfTheTheodosia
          $ labeled "Put the set-aside Deck of the Theodosia location into play."
          . placeLocation_
        for_ mUniversityOfHalls
          $ labeled "Put the set-aside University Halls location into play."
          . placeLocation_

      pure s
    _ -> PrisonOfMemories <$> liftRunMessage msg attrs
