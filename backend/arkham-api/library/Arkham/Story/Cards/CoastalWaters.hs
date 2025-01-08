module Arkham.Story.Cards.CoastalWaters (coastalWaters) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype CoastalWaters = CoastalWaters StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coastalWaters :: StoryCard CoastalWaters
coastalWaters = story CoastalWaters Cards.coastalWaters

instance RunMessage CoastalWaters where
  runMessage msg s@(CoastalWaters attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mAirfield <- getSetAsideCardMaybe Locations.airfield
      mOttomanFront <- getSetAsideCardMaybe Locations.ottomanFront

      chooseOneM iid do
        for_ mAirfield
          $ labeled "Put the set-aside Airfield location into play."
          . placeLocation_
        for_ mOttomanFront
          $ labeled "Put the set-aside Ottoman Front location into play."
          . placeLocation_
      pure s
    _ -> CoastalWaters <$> liftRunMessage msg attrs
