module Arkham.Story.Cards.TheDreamEaters.PointOfNoReturn.StillSurface (StillSurface (..), stillSurface) where

import Arkham.Location.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Locations
import Arkham.Matcher
import Arkham.Source
import Arkham.Story.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Cards
import Arkham.Story.Import.Lifted

newtype StillSurface = StillSurface StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stillSurface :: StoryCard StillSurface
stillSurface = story StillSurface Cards.stillSurface

instance RunMessage StillSurface where
  runMessage msg s@(StillSurface attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      -- Look at the other side of another copy of Sea of Pitch.
      seas <- select $ LocationWithTitle "Sea of Pitch" <> not_ (locationIs Locations.seaOfPitch_263)
      unless (null seas) do
        chooseOne iid [targetLabel sea [LookAtRevealed iid (toSource attrs) (toTarget sea)] | sea <- seas]
      pure s
    _ -> StillSurface <$> liftRunMessage msg attrs
