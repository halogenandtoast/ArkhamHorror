module Arkham.Story.Cards.TheInfestationBegins (
  TheInfestationBegins (..),
  theInfestationBegins,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (checkWindows)
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Scenarios.WakingNightmare.InfestationBag
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Trait (Trait (Spider))
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window
import Arkham.Zone
import Data.Aeson.KeyMap ((!?))

newtype TheInfestationBegins = TheInfestationBegins StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

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
      lead <- getLead
      (tokens, rest) <- splitAt 1 <$> shuffleM (infestationTokens bag)
      let token = fromJustNote "invalid infestation token" $ headMay tokens
      let bag' = bag {infestationTokens = rest, infestationCurrentToken = Just token}
      revealWindow <- checkWindows [mkWhen (Window.RevealChaosToken lead $ asChaosToken token)]
      pushAll
        [ FocusChaosTokens [asChaosToken token]
        , revealWindow
        , SendMessage (toTarget attrs) (ResolveChaosToken (asChaosToken token) token.face lead)
        ]

      pure
        $ TheInfestationBegins
        $ attrs {storyMeta = toJSON bag'}
    SendMessage (isTarget attrs -> True) (ResolveChaosToken _ _ _) | Just token <- infestationCurrentToken (infestationBag attrs) -> do
      let tokenFace = token.face
      mods <- getModifiers attrs
      let bag =
            (infestationBag attrs)
              { infestationCurrentToken = Nothing
              , infestationSetAside = infestationSetAside (infestationBag attrs) <> [InfestationToken tokenFace]
              }
      let
        enabled = \case
          MetaModifier (Object o) -> o !? "treatTabletAsSkill" == Just (Bool True)
          _ -> False
      let treatTabletAsSkull = any enabled mods
      let swapToken = \case
            Tablet | treatTabletAsSkull -> Skull
            t -> t

      case swapToken tokenFace of
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
                ( \location ->
                    selectList
                      $ NotLocation (oneOf [InfestedLocation, LocationWithHorror $ atLeast 1])
                      <> ConnectedFrom (LocationWithId location)
                )
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

      lead <- getLeadPlayer
      push $ chooseOne lead [Label "Continue" [UnfocusChaosTokens]]

      if count (== InfestationToken Cultist) (infestationTokens bag) == 2
        then do
          pure
            $ TheInfestationBegins
            $ attrs {storyMeta = toJSON initInfestationBag}
        else pure $ TheInfestationBegins $ attrs {storyMeta = toJSON bag}
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
    SendMessage (isTarget attrs -> True) (ChaosTokenCanceled _ _ _) -> do
      let bag = infestationBag attrs
      let bag' =
            bag
              { infestationTokens = infestationTokens bag <> maybeToList (infestationCurrentToken bag)
              , infestationCurrentToken = Nothing
              }
      push $ UnfocusChaosTokens
      pure $ TheInfestationBegins $ attrs {storyMeta = toJSON bag'}
    _ -> TheInfestationBegins <$> runMessage msg attrs
