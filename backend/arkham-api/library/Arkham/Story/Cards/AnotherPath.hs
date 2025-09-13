module Arkham.Story.Cards.AnotherPath (anotherPath) where

import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Log
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AnotherPath = AnotherPath StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherPath :: StoryCard AnotherPath
anotherPath = story AnotherPath Cards.anotherPath

instance RunMessage AnotherPath where
  runMessage msg s@(AnotherPath attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      placeSetAsideLocation_ Locations.enchantedWoodsStoneTrapdoor
      record TheInvestigatorsFoundAWayOutOfTheUnderworld
      pure s
    _ -> AnotherPath <$> liftRunMessage msg attrs
