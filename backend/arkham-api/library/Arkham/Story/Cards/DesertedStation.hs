module Arkham.Story.Cards.DesertedStation (desertedStation) where

import Arkham.Helpers.Query
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
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mAlaskanWilds <- getSetAsideCardMaybe Locations.alaskanWilds

      chooseOneM iid do
        for_ mAlaskanWilds
          $ labeled "Put the set-aside Alaskan Wilds location into play."
          . placeLocation_
      pure s
    _ -> DesertedStation <$> liftRunMessage msg attrs
