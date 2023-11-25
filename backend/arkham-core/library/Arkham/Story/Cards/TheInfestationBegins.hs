module Arkham.Story.Cards.TheInfestationBegins (
  TheInfestationBegins (..),
  theInfestationBegins,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Trait (Trait (Spider))
import Arkham.Zone
import Data.Aeson (Result (..))
import Data.UUID (nil)

newtype TheInfestationBegins = TheInfestationBegins StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInfestationBegins :: StoryCard TheInfestationBegins
theInfestationBegins = story TheInfestationBegins Cards.theInfestationBegins

infestationBag :: StoryAttrs -> InfestationBag
infestationBag attrs = case fromJSON (storyMeta attrs) of
  Success a -> a
  _ -> error "invalid infestation bag"

newtype InfestationToken = InfestationToken {infestationTokenFace :: ChaosTokenFace}
  deriving newtype (Show, Eq, ToJSON, FromJSON)

data InfestationBag = InfestationBag
  { infestationTokens :: [InfestationToken]
  , infestationSetAside :: [InfestationToken]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

initInfestationBag :: InfestationBag
initInfestationBag =
  InfestationBag
    { infestationTokens =
        map InfestationToken [#skull, #tablet, #tablet, #tablet, #tablet, #cultist, #cultist]
    , infestationSetAside = []
    }

instance RunMessage TheInfestationBegins where
  runMessage msg s@(TheInfestationBegins attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      locationsWithMostClues <- selectList $ LocationWithMostClues Anywhere
      lead <- getLeadPlayer
      playerCount <- getPlayerCount
      pushAll
        $ [ chooseOrRunOne
              lead
              [ targetLabel location [PlaceTokens (StorySource $ toId attrs) (toTarget location) #damage 1]
              | location <- locationsWithMostClues
              ]
          ]
        <> [DoStep 1 msg | playerCount >= 3]
      pure
        $ TheInfestationBegins
        $ attrs {storyFlipped = True, storyMeta = toJSON initInfestationBag}
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
    SendMessage (isTarget attrs -> True) (RequestChaosTokens _ _ (Reveal 1) _) -> do
      let bag = infestationBag attrs
      (tokens, rest) <- splitAt 1 <$> shuffleM (infestationTokens bag)
      let token = fromJustNote "invalid infestation token" $ headMay tokens
      case infestationTokenFace token of
        Skull -> do
          lead <- getLead
          push
            $ FindEncounterCard
              lead
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard]
              (#enemy <> withTrait Spider)
        Tablet -> do
          pure ()
        Cultist -> do
          infestedLocations <- selectList InfestedLocation
          adjacentLocations <-
            nub
              . concat
              <$> for
                infestedLocations
                (\location -> selectList $ NotLocation InfestedLocation <> ConnectedFrom (LocationWithId location))
          when (notNull adjacentLocations) $ do
            lead <- getLeadPlayer
            push
              $ chooseOne lead
              $ [ targetLabel
                  location
                  [ PlaceTokens (toSource attrs) (toTarget location) #damage 1
                  ]
                | location <- adjacentLocations
                ]
        _ -> error "Invalid infestation token"
      let bag' = bag {infestationTokens = rest, infestationSetAside = tokens <> infestationSetAside bag}

      lead <- getLeadPlayer
      pushAll
        [ FocusChaosTokens [ChaosToken (ChaosTokenId nil) (infestationTokenFace token)]
        , chooseOne lead [Label "Continue" [UnfocusChaosTokens]]
        ]

      if count (== InfestationToken Cultist) (infestationTokens bag') == 2
        then do
          pure
            $ TheInfestationBegins
            $ attrs {storyMeta = toJSON initInfestationBag}
        else
          pure
            $ TheInfestationBegins
            $ attrs {storyMeta = toJSON bag'}
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      investigators <- getInvestigators
      locations <-
        nub . concat <$> for investigators \i -> selectList (NearestLocationTo i InfestedLocation)
      lead <- getLeadPlayer
      locationsWithCreation <- for locations $ \location -> do
        (location,) <$> createEnemyAt_ card location Nothing

      push
        $ chooseOrRunOne
          lead
          [targetLabel location [creation] | (location, creation) <- locationsWithCreation]
      pure s
    SendMessage (isTarget attrs -> True) (AddChaosToken face) -> do
      let bag = infestationBag attrs
      let bag' = bag {infestationTokens = InfestationToken face : infestationTokens bag}
      pure $ TheInfestationBegins $ attrs {storyMeta = toJSON bag'}
    _ -> TheInfestationBegins <$> runMessage msg attrs
