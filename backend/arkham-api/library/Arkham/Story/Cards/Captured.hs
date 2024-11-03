module Arkham.Story.Cards.Captured (Captured (..), captured) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Window qualified as Window

newtype Captured = Captured StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captured :: StoryCard Captured
captured = story Captured Cards.captured

instance RunMessage Captured where
  runMessage msg s@(Captured attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    ForInvestigator iid (ScenarioSpecific "captured" _) -> do
      assets <- select $ assetControlledBy iid <> #hand
      for_ assets $ returnToHand iid
      removeFromGame attrs
      holdingCells <- placeLocationCardInGrid (Pos 5 1) Locations.holdingCells
      place iid holdingCells
      checkWhen $ Window.ScenarioEvent "captured" (toJSON iid)
      pure s
    _ -> Captured <$> liftRunMessage msg attrs
