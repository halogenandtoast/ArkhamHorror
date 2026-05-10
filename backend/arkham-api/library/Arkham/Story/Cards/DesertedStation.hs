module Arkham.Story.Cards.DesertedStation (desertedStation) where

import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DesertedStation = DesertedStation StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desertedStation :: StoryCard DesertedStation
desertedStation = story DesertedStation Cards.desertedStation

instance RunMessage DesertedStation where
  runMessage msg s@(DesertedStation attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      mAlaskanWilds <- getSetAsideCardMaybe Locations.alaskanWilds

      chooseOneM iid do
        for_ mAlaskanWilds \loc ->
          withI18n $ keyVar "name" "Alaskan Wilds" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
      pure s
    _ -> DesertedStation <$> liftRunMessage msg attrs
