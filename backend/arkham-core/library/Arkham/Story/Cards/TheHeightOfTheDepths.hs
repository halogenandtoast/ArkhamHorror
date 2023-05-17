module Arkham.Story.Cards.TheHeightOfTheDepths (
  TheHeightOfTheDepths (..),
  theHeightOfTheDepths,
) where

import Arkham.Prelude

import Arkham.Helpers.Investigator
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheHeightOfTheDepths = TheHeightOfTheDepths StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeightOfTheDepths :: StoryCard TheHeightOfTheDepths
theHeightOfTheDepths = story TheHeightOfTheDepths Cards.theHeightOfTheDepths

instance RunMessage TheHeightOfTheDepths where
  runMessage msg s@(TheHeightOfTheDepths attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      healHorrorMessages <-
        map snd <$> getInvestigatorsWithHealHorror attrs 5 Anyone
      setAsideDepthsOfDemhe <-
        getSetAsideCardsMatching $
          CardWithTitle "Depths of Demhe"
      otherDepthsOfDemhe <- case nonEmpty setAsideDepthsOfDemhe of
        Nothing -> error "missing"
        Just xs -> sample xs
      depthsOfDemhe <- selectJust $ locationIs Locations.depthsOfDemheTheHeightOfTheDepths
      pushAll $
        healHorrorMessages
          <> [ReplaceLocation depthsOfDemhe otherDepthsOfDemhe DefaultReplace]
      pure s
    _ -> TheHeightOfTheDepths <$> runMessage msg attrs
