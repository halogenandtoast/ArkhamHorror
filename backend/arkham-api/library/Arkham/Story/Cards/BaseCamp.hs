module Arkham.Story.Cards.BaseCamp (baseCamp) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype BaseCamp = BaseCamp StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseCamp :: StoryCard BaseCamp
baseCamp = story BaseCamp Cards.baseCamp

instance RunMessage BaseCamp where
  runMessage msg s@(BaseCamp attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mCoastalWaters <- getSetAsideCardMaybe Locations.coastalWaters
      mDesertedStation <- getSetAsideCardMaybe Locations.desertedStation
      mRiverviewTheatre <- getSetAsideCardMaybe Locations.riverviewTheatre

      chooseOneM iid do
        for_ mCoastalWaters
          $ labeled "Put the set-aside Coastal Waters location into play."
          . placeLocation_
        for_ mDesertedStation
          $ labeled "Put the set-aside Deserted Station location into play."
          . placeLocation_
        for_ mRiverviewTheatre
          $ labeled "Put the set-aside Riverview Theatre location into play."
          . placeLocation_

      pure s
    _ -> BaseCamp <$> liftRunMessage msg attrs
