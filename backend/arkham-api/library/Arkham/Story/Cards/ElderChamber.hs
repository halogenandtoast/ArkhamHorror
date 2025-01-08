module Arkham.Story.Cards.ElderChamber (elderChamber) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ElderChamber = ElderChamber StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elderChamber :: StoryCard ElderChamber
elderChamber = story ElderChamber Cards.elderChamber

instance RunMessage ElderChamber where
  runMessage msg s@(ElderChamber attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mClutteredDormitory <- getSetAsideCardMaybe Locations.clutteredDormitory

      chooseOneM iid do
        for_ mClutteredDormitory
          $ labeled "Put the set-aside Cluttered Dormitory location into play."
          . placeLocation_
      pure s
    _ -> ElderChamber <$> liftRunMessage msg attrs
