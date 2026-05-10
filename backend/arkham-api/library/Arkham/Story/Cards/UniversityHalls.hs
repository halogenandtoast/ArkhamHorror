module Arkham.Story.Cards.UniversityHalls (universityHalls) where

import Arkham.Helpers.Query
import Arkham.I18n
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
    ResolveThisStory iid (is attrs -> True) -> do
      mElderChamber <- getSetAsideCardMaybe Locations.elderChamber
      mRiverviewTheatre <- getSetAsideCardMaybe Locations.riverviewTheatre
      mStandingStones <- getSetAsideCardMaybe Locations.standingStones

      chooseOneM iid do
        for_ mElderChamber \loc ->
          withI18n $ keyVar "name" "Elder Chamber" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mRiverviewTheatre \loc ->
          withI18n $ keyVar "name" "Riverview Theatre" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mStandingStones \loc ->
          withI18n $ keyVar "name" "Standing Stones" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
      pure s
    _ -> UniversityHalls <$> liftRunMessage msg attrs
