module Arkham.Story.Cards.HedgeMaze (hedgeMaze) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype HedgeMaze = HedgeMaze StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hedgeMaze :: StoryCard HedgeMaze
hedgeMaze = story HedgeMaze Cards.hedgeMaze

instance RunMessage HedgeMaze where
  runMessage msg s@(HedgeMaze attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      mMoaiStatues <- getSetAsideCardMaybe Locations.moaiStatues

      chooseOneM iid do
        for_ mMoaiStatues
          $ labeled "Put the set-aside Mo'ai Statues location into play."
          . placeLocation_
      pure s
    _ -> HedgeMaze <$> liftRunMessage msg attrs
