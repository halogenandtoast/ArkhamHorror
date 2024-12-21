module Arkham.Story.Cards.StandingStones (standingStones) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype StandingStones = StandingStones StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

standingStones :: StoryCard StandingStones
standingStones = story StandingStones Cards.standingStones

instance RunMessage StandingStones where
  runMessage msg s@(StandingStones attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mTheBlackStone <- getSetAsideCardMaybe Locations.theBlackStone
      mDyersClassroom <- getSetAsideCardMaybe Locations.dyersClassroom

      chooseOneM iid do
        for_ mTheBlackStone
          $ labeled "Put the set-aside The Black Stone location into play."
          . placeLocation_
        for_ mDyersClassroom
          $ labeled "Put the set-aside Dyer's Classroom location into play."
          . placeLocation_

      pure s
    _ -> StandingStones <$> liftRunMessage msg attrs
