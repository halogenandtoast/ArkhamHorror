module Arkham.Story.Cards.RiverviewTheatre (riverviewTheatre) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RiverviewTheatre = RiverviewTheatre StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverviewTheatre :: StoryCard RiverviewTheatre
riverviewTheatre = story RiverviewTheatre Cards.riverviewTheatre

instance RunMessage RiverviewTheatre where
  runMessage msg s@(RiverviewTheatre attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mDrKenslersOffice <- getSetAsideCardMaybe Locations.drKenslersOffice
      mInfirmary <- getSetAsideCardMaybe Locations.infirmaryFatalMirage

      chooseOneM iid do
        for_ mDrKenslersOffice
          $ labeled "Put the set-aside Dr. Kensler's Office location into play."
          . placeLocation_
        for_ mInfirmary
          $ labeled "Put the set-aside Infirmary location into play."
          . placeLocation_

      pure s
    _ -> RiverviewTheatre <$> liftRunMessage msg attrs
