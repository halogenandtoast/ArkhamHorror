module Arkham.Story.Cards.StillSurface (StillSurface (..), stillSurface) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target

newtype StillSurface = StillSurface StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stillSurface :: StoryCard StillSurface
stillSurface = story StillSurface Cards.stillSurface

instance RunMessage StillSurface where
  runMessage msg s@(StillSurface attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      -- Look at the other side of another copy of Sea of Pitch.
      seas <-
        select
          $ LocationWithTitle "Sea of Pitch"
          <> LocationCanBeFlipped
          <> not_ (locationIs Locations.seaOfPitch_262)
      unless (null seas) do
        chooseOne iid [targetLabel sea [Flip iid (toSource attrs) (toTarget sea)] | sea <- seas]
      pure s
    _ -> StillSurface <$> liftRunMessage msg attrs
