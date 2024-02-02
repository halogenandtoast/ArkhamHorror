module Arkham.Story.Cards.ThePalaceOfRainbows (ThePalaceOfRainbows (..), thePalaceOfRainbows) where

import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype ThePalaceOfRainbows = ThePalaceOfRainbows StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

thePalaceOfRainbows :: StoryCard ThePalaceOfRainbows
thePalaceOfRainbows = story ThePalaceOfRainbows Cards.thePalaceOfRainbows

instance RunMessage ThePalaceOfRainbows where
  runMessage msg s@(ThePalaceOfRainbows attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      iids <- selectList $ InvestigatorAt (locationIs Locations.ilekVad)
      for_ iids \iid -> do
        mDrawing <- drawCardsIfCan iid (toSource attrs) 2
        mHeal <- getHealHorrorMessage attrs 2 iid
        for_ mDrawing push
        for_ mHeal push
      pure s
    _ -> ThePalaceOfRainbows <$> runMessage msg attrs
