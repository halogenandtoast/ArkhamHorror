module Arkham.Story.Cards.BaseCamp (baseCamp) where

import Arkham.Helpers.Query
import Arkham.I18n
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
    ResolveThisStory iid (is attrs -> True) -> do
      mCoastalWaters <- getSetAsideCardMaybe Locations.coastalWaters
      mDesertedStation <- getSetAsideCardMaybe Locations.desertedStation
      mRiverviewTheatre <- getSetAsideCardMaybe Locations.riverviewTheatre

      chooseOneM iid do
        for_ mCoastalWaters \loc ->
          withI18n $ keyVar "name" "Coastal Waters" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mDesertedStation \loc ->
          withI18n $ keyVar "name" "Deserted Station" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mRiverviewTheatre \loc ->
          withI18n $ keyVar "name" "Riverview Theatre" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc

      pure s
    _ -> BaseCamp <$> liftRunMessage msg attrs
