module Arkham.Scenarios.WakingNightmare.Helpers where

import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Target

pattern InfestedLocation :: LocationMatcher
pattern InfestedLocation <- (LocationWithDamage (GreaterThan (Static 0)))
  where
    InfestedLocation = LocationWithDamage (GreaterThan (Static 0))

makeInfestationTest :: (HasQueue Message m, HasGame m, Query StoryMatcher) => m ()
makeInfestationTest = do
  theInfestationBegins <- selectJust $ storyIs Stories.theInfestationBegins
  push
    $ SendMessage
      (StoryTarget theInfestationBegins)
      (RequestChaosTokens (StorySource theInfestationBegins) Nothing (Reveal 1) SetAside)
