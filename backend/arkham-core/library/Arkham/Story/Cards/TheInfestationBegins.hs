module Arkham.Story.Cards.TheInfestationBegins (
  TheInfestationBegins (..),
  theInfestationBegins,
) where

import Arkham.Prelude

import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheInfestationBegins = TheInfestationBegins StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInfestationBegins :: StoryCard TheInfestationBegins
theInfestationBegins = story TheInfestationBegins Cards.theInfestationBegins

instance RunMessage TheInfestationBegins where
  runMessage msg s@(TheInfestationBegins attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      locationsWithMostClues <- selectList $ LocationWithMostClues Anywhere
      lead <- getLeadPlayer
      playerCount <- getPlayerCount
      pushAll
        $ [ chooseOrRunOne
              lead
              [ targetLabel location [PlaceTokens (toSource attrs) (toTarget location) #damage 1]
              | location <- locationsWithMostClues
              ]
          ]
        <> [DoStep 1 msg | playerCount >= 3]
      pure $ TheInfestationBegins $ attrs {storyFlipped = True}
    DoStep _ (ResolveStory _ ResolveIt story') | story' == toId attrs -> do
      locationsWithMostClues <- selectList $ LocationWithMostClues $ NotLocation InfestedLocation
      lead <- getLeadPlayer
      pushAll
        [ chooseOrRunOne
            lead
            [ targetLabel location [PlaceTokens (toSource attrs) (toTarget location) #damage 1]
            | location <- locationsWithMostClues
            ]
        ]
      pure s
    _ -> TheInfestationBegins <$> runMessage msg attrs
