module Arkham.Scenarios.WakingNightmare.Helpers where

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Target
import Arkham.Timing (Timing)

pattern InfestedLocation :: LocationMatcher
pattern InfestedLocation <- (LocationWithDamage (GreaterThan (Static 0)))
  where
    InfestedLocation = LocationWithDamage (GreaterThan (Static 0))

pattern BecomesInfested :: Timing -> LocationMatcher -> WindowMatcher
pattern BecomesInfested t lmatcher <-
  PlacedCounterOnLocation t lmatcher AnySource DamageCounter (GreaterThan (Static 0))
  where
    BecomesInfested t lmatcher = PlacedCounterOnLocation t lmatcher AnySource DamageCounter (GreaterThan (Static 0))

makeInfestationTest :: (HasGame m, Query StoryMatcher) => m Message
makeInfestationTest = do
  theInfestationBegins <- selectJust $ storyIs Stories.theInfestationBegins
  pure
    $ SendMessage
      (StoryTarget theInfestationBegins)
      (RequestChaosTokens (StorySource theInfestationBegins) Nothing (Reveal 1) SetAside)

addInfestationToken
  :: (HasGame m, Query StoryMatcher) => ChaosTokenFace -> m Message
addInfestationToken face = do
  theInfestationBegins <- selectJust $ storyIs Stories.theInfestationBegins
  pure $ SendMessage (StoryTarget theInfestationBegins) (AddChaosToken face)
