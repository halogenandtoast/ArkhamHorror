module Arkham.Story.Cards.CoastalWaters (coastalWaters) where

import Arkham.Helpers.Query
import Arkham.I18n
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
    ResolveThisStory iid (is attrs -> True) -> do
      mAirfield <- getSetAsideCardMaybe Locations.airfield
      mOttomanFront <- getSetAsideCardMaybe Locations.ottomanFront

      chooseOneM iid do
        for_ mAirfield \loc ->
          withI18n $ keyVar "name" "Airfield" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mOttomanFront \loc ->
          withI18n $ keyVar "name" "Ottoman Front" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
      pure s
    _ -> CoastalWaters <$> liftRunMessage msg attrs
