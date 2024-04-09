module Arkham.Story.Cards.ThePalaceOfRainbows (ThePalaceOfRainbows (..), thePalaceOfRainbows) where

import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype ThePalaceOfRainbows = ThePalaceOfRainbows StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePalaceOfRainbows :: StoryCard ThePalaceOfRainbows
thePalaceOfRainbows = story ThePalaceOfRainbows Cards.thePalaceOfRainbows

instance RunMessage ThePalaceOfRainbows where
  runMessage msg s@(ThePalaceOfRainbows attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      iids <- select $ InvestigatorAt (locationIs Locations.ilekVad)
      for_ iids \iid -> do
        mDrawing <- drawCardsIfCan iid (toSource attrs) 2
        canHeal <- canHaveHorrorHealed (toSource attrs) iid
        for_ mDrawing push
        pushWhen canHeal $ HealHorror (toTarget iid) (toSource attrs) 2
      pure s
    _ -> ThePalaceOfRainbows <$> runMessage msg attrs
