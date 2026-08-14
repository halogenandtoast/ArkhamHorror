module Arkham.Scenarios.WakingNightmare.Helpers where

import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.GameValue
import Arkham.Helpers.FlavorText (FlavorTextBuilder, p, setTitle)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Target
import Arkham.Timing (Timing)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "wakingNightmare" a

scenarioFlavorText :: Scope -> FlavorTextBuilder ()
scenarioFlavorText entry = scenarioI18n $ scope "flavorText" $ scope entry do
  setTitle "title"
  p "body"

pattern InfestedLocation :: LocationMatcher
pattern InfestedLocation <- (LocationWithDamage (GreaterThan (Static 0)))
  where
    InfestedLocation = LocationWithDamage (GreaterThan (Static 0))

pattern BecomesInfested :: Timing -> LocationMatcher -> WindowMatcher
pattern BecomesInfested t lmatcher <-
  PlacedCounterOnLocation t lmatcher AnySource DamageCounter (GreaterThan (Static 0))
  where
    BecomesInfested t lmatcher = PlacedCounterOnLocation t lmatcher AnySource DamageCounter (GreaterThan (Static 0))

makeInfestationTest :: ReverseQueue m => m ()
makeInfestationTest = do
  theInfestationBegins <- selectJust $ storyIs Stories.theInfestationBegins
  push
    $ SendMessage
      (StoryTarget theInfestationBegins)
      (RequestChaosTokens (StorySource theInfestationBegins) Nothing (Reveal 1) SetAside)

addInfestationToken :: HasGame m => ChaosTokenFace -> m Message
addInfestationToken face = do
  theInfestationBegins <- selectJust $ storyIs Stories.theInfestationBegins
  pure $ SendMessage (StoryTarget theInfestationBegins) (AddChaosToken face)
