module Arkham.Story.Cards.ThePalaceOfRainbows (thePalaceOfRainbows) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ThePalaceOfRainbows = ThePalaceOfRainbows StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePalaceOfRainbows :: StoryCard ThePalaceOfRainbows
thePalaceOfRainbows = story ThePalaceOfRainbows Cards.thePalaceOfRainbows

instance RunMessage ThePalaceOfRainbows where
  runMessage msg s@(ThePalaceOfRainbows attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      iids <- select $ InvestigatorAt (locationIs Locations.ilekVad)
      for_ iids \iid -> do
        drawCards iid attrs 2
        healHorrorIfCan iid attrs 2
      pure s
    _ -> ThePalaceOfRainbows <$> liftRunMessage msg attrs
