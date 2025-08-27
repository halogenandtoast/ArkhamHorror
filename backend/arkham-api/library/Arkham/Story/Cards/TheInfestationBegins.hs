module Arkham.Story.Cards.TheInfestationBegins (theInfestationBegins) where

import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Distance
import {-# SOURCE #-} Arkham.GameEnv (getDistance)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getInvestigators, getLead, getPlayerCount)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Scenarios.WakingNightmare.InfestationBag
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Story.Types (StoryAttrs (..))
import Arkham.Trait (Trait (Spider))
import Arkham.Window qualified as Window
import Data.Aeson.KeyMap ((!?))

newtype TheInfestationBegins = TheInfestationBegins StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInfestationBegins :: StoryCard TheInfestationBegins
theInfestationBegins =
  storyWith TheInfestationBegins Cards.theInfestationBegins \a -> a {storyRemoveAfterResolution = False}

instance RunMessage TheInfestationBegins where
  runMessage msg s@(TheInfestationBegins attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      locationsWithMostClues <- select $ LocationWithMostClues Anywhere
      playerCount <- getPlayerCount
      leadChooseOrRunOneM do
        targets locationsWithMostClues $ placeTokensOn attrs #damage 1
      when (playerCount >= 3) $ doStep 1 msg
      bag <- initInfestationBag
      pure
        $ TheInfestationBegins
        $ attrs {storyFlipped = True, storyMeta = toJSON bag}
    DoStep _ (ResolveThisStory _ (is attrs -> True)) -> do
      locationsWithMostClues <- select $ LocationWithMostClues $ NotLocation InfestedLocation
      leadChooseOrRunOneM $ targets locationsWithMostClues $ placeTokensOn attrs #damage 1
      pure s
    SendMessage (isTarget attrs -> True) (RequestChaosTokens _ _ (Reveal 1) _) -> do
      let bag = infestationBag attrs
      lead <- getLead
      (tokens, rest) <- splitAt 1 <$> shuffleM (infestationTokens bag)
      let token = fromJustNote "invalid infestation token" $ headMay tokens
      let bag' = bag {infestationTokens = rest, infestationCurrentToken = Just token}
      focusChaosTokens [asChaosToken token] \unfocus -> do
        checkWhen $ Window.RevealChaosToken lead $ asChaosToken token
        checkWhen
          $ Window.ScenarioEvent
            ("revealInfestationToken:" <> tshow token.face)
            (Just lead)
            (toJSON $ asChaosToken token)
        push unfocus
        sendMessage attrs $ ResolveChaosToken (asChaosToken token) token.face lead

      pure
        $ TheInfestationBegins
        $ attrs {storyMeta = toJSON bag'}
    SendMessage (isTarget attrs -> True) (ResolveChaosToken {}) | Just token <- attrs.infestationBag.currentToken -> do
      let tokenFace = token.face
      send $ format (asChaosToken token) <> " drawn during Infestation Test"
      mods <- getModifiers attrs
      let bag =
            attrs.infestationBag
              { infestationCurrentToken = Nothing
              , infestationSetAside = attrs.infestationBag.setAside <> [token]
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
          findEncounterCard lead attrs (card_ $ #enemy <> withTrait Spider)
        Tablet -> pure ()
        Cultist -> do
          infestedLocations <- select InfestedLocation
          adjacentLocations <-
            nub . concat <$> for infestedLocations \location ->
              select
                $ not_ (oneOf [InfestedLocation, LocationWithHorror $ atLeast 1])
                <> ConnectedFrom (LocationWithId location)
          leadChooseOneM $ targets adjacentLocations $ placeTokensOn attrs #damage 1
        _ -> error "Invalid infestation token"

      leadChooseOneM $ labeled "Continue" nothing

      if count ((== Cultist) . infestationTokenFace) (infestationSetAside bag) == 2
        then do
          bag' <- initInfestationBag
          pure
            $ TheInfestationBegins
            $ attrs {storyMeta = toJSON bag'}
        else pure $ TheInfestationBegins $ attrs {storyMeta = toJSON bag}
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      investigators <- getInvestigators
      locations <-
        nub . mins . concat <$> for investigators \i -> do
          select (NearestLocationTo i InfestedLocation) >>= \case
            xs@(x : _) ->
              fromMaybe [] <$> runMaybeT do
                lid <- MaybeT $ getMaybeLocation i
                d <- MaybeT $ getDistance x lid
                pure $ map (,unDistance d) xs
            _ -> pure []

      leadChooseOneM $ targets locations (createEnemyAt_ card)
      pure s
    SendMessage (isTarget attrs -> True) (AddChaosToken face) -> do
      let bag = infestationBag attrs
      tokenId <- getRandom
      let bag' = bag {infestationTokens = InfestationToken tokenId face : bag.tokens}
      pure $ TheInfestationBegins $ attrs {storyMeta = toJSON bag'}
    SendMessage (isTarget attrs -> True) (ChaosTokenCanceled {}) -> do
      let bag = infestationBag attrs
      let bag' =
            bag
              { infestationTokens = bag.tokens <> maybeToList bag.currentToken
              , infestationCurrentToken = Nothing
              }
      pure $ TheInfestationBegins $ attrs {storyMeta = toJSON bag'}
    _ -> TheInfestationBegins <$> liftRunMessage msg attrs
