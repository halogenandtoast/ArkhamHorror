module Arkham.Story.Cards.StandingStones (standingStones) where

import Arkham.Helpers.Query
import Arkham.I18n
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
    ResolveThisStory iid (is attrs -> True) -> do
      mTheBlackStone <- getSetAsideCardMaybe Locations.theBlackStone
      mDyersClassroom <- getSetAsideCardMaybe Locations.dyersClassroom

      chooseOneM iid do
        for_ mTheBlackStone \loc ->
          withI18n $ keyVar "name" "The Black Stone" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mDyersClassroom \loc ->
          withI18n $ keyVar "name" "Dyer's Classroom" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc

      pure s
    _ -> StandingStones <$> liftRunMessage msg attrs
