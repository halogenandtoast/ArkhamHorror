module Arkham.Scenarios.WrittenInRock.Helpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Classes.HasQueue
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher.Location
import Arkham.Message
import Arkham.Message.Lifted.Queue
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "writtenInRock" a

pattern CannotBeSlidOrSwapped :: ModifierType
pattern CannotBeSlidOrSwapped = ScenarioModifier "cannotBeSlidOrSwapped"

pattern LocationCanBeSwapped :: LocationMatcher
pattern LocationCanBeSwapped = LocationWithoutModifier CannotBeSlidOrSwapped

pattern LocationCanBeSlid :: LocationMatcher
pattern LocationCanBeSlid = LocationWithoutModifier CannotBeSlidOrSwapped

swapLocations :: ReverseQueue m => LocationId -> LocationId -> m ()
swapLocations lid1 lid2 = do
  pos1 <- fieldJust LocationPosition lid1
  pos2 <- fieldJust LocationPosition lid2
  push $ PlaceGrid (GridLocation pos2 lid1)
  push $ PlaceGrid (GridLocation pos1 lid2)
