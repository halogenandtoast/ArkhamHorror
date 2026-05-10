module Arkham.Story.Cards.RiverviewTheatre (riverviewTheatre) where

import Arkham.Helpers.Query
import Arkham.I18n
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
    ResolveThisStory iid (is attrs -> True) -> do
      mDrKenslersOffice <- getSetAsideCardMaybe Locations.drKenslersOffice
      mInfirmary <- getSetAsideCardMaybe Locations.infirmaryFatalMirage

      chooseOneM iid do
        for_ mDrKenslersOffice \loc ->
          withI18n $ keyVar "name" "Dr. Kensler's Office" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mInfirmary \loc ->
          withI18n $ keyVar "name" "Infirmary" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc

      pure s
    _ -> RiverviewTheatre <$> liftRunMessage msg attrs
