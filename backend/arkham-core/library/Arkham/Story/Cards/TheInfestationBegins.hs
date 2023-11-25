module Arkham.Story.Cards.TheInfestationBegins (
  TheInfestationBegins (..),
  theInfestationBegins,
) where

import Arkham.Prelude

import Arkham.ChaosBag
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Data.Aeson (Result (..))

newtype TheInfestationBegins = TheInfestationBegins StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInfestationBegins :: StoryCard TheInfestationBegins
theInfestationBegins = story TheInfestationBegins Cards.theInfestationBegins

infestationBag :: StoryAttrs -> ChaosBag
infestationBag attrs = case fromJSON (storyMeta attrs) of
  Success a -> a
  _ -> error "invalid infestation bag"

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
      bag <-
        foldM
          (\b c -> runMessage (AddChaosToken c) b)
          emptyChaosBag
          [#skull, #tablet, #tablet, #tablet, #tablet, #cultist, #cultist]
      pure
        $ TheInfestationBegins
        $ attrs {storyFlipped = True, storyMeta = toJSON bag}
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
    SendMessage (isTarget attrs -> True) msg' -> do
      let bag = infestationBag attrs
      bag' <- runMessage msg' bag
      pure
        $ TheInfestationBegins
        $ attrs {storyMeta = toJSON bag'}
    _ -> TheInfestationBegins <$> runMessage msg attrs
