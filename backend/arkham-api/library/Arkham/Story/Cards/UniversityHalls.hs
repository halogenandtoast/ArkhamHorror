module Arkham.Story.Cards.UniversityHalls (universityHalls) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype UniversityHalls = UniversityHalls StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

universityHalls :: StoryCard UniversityHalls
universityHalls = story UniversityHalls Cards.universityHalls

instance RunMessage UniversityHalls where
  runMessage msg s@(UniversityHalls attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mElderChamber <- getSetAsideCardMaybe Locations.elderChamber
      mRiverviewTheatre <- getSetAsideCardMaybe Locations.riverviewTheatre
      mStandingStones <- getSetAsideCardMaybe Locations.standingStones

      chooseOneM iid do
        for_ mElderChamber
          $ labeled "Put the set-aside Elder Chamber location into play."
          . placeLocation_
        for_ mRiverviewTheatre
          $ labeled "Put the set-aside Riverview Theatre location into play."
          . placeLocation_
        for_ mStandingStones
          $ labeled "Put the set-aside Standing Stones location into play."
          . placeLocation_
      pure s
    _ -> UniversityHalls <$> liftRunMessage msg attrs
