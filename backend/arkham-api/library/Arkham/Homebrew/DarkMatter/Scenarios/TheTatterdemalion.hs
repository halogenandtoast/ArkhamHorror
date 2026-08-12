module Arkham.Homebrew.DarkMatter.Scenarios.TheTatterdemalion (theTatterdemalion) where

import Arkham.Card (toCardDef)
import Arkham.ChaosToken
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.DarkMatter.Helpers (addScanningDeck, earnXp, scenarioI18n)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.Traits (pattern AI)
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype TheTatterdemalion = TheTatterdemalion ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTatterdemalion :: Difficulty -> TheTatterdemalion
theTatterdemalion difficulty =
  scenario
    TheTatterdemalion
    ":dark-matter:014"
    "The Tatterdemalion"
    difficulty
    [ "moon      moon      .      plus     plus     equals equals"
    , ".         square    square circle   circle   t      t"
    , "hourglass hourglass .      triangle triangle .      ."
    ]

{- | Easy/Standard: [skull] -X where X is the number of [[AI]] encounter cards in
your threat area; [cultist] -2.
Hard/Expert: [skull] -X where X is the number of [[AI]] encounter cards in
play; [cultist] -3.

The [cultist] rider ("If you fail, place 1 of your clues onto your current
location") is handled by the 'FailedSkillTest' case in 'RunMessage' below.
-}
instance HasChaosTokenValue TheTatterdemalion where
  getChaosTokenValue iid tokenFace (TheTatterdemalion attrs) = case tokenFace of
    Skull -> do
      n <-
        if isEasyStandard attrs
          then selectCount $ TreacheryWithTrait AI <> TreacheryInThreatAreaOf (InvestigatorWithId iid)
          else selectCount $ TreacheryWithTrait AI <> InPlayTreachery
      pure $ ChaosTokenValue Skull (NegativeModifier n)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheTatterdemalion where
  runMessage msg s@(TheTatterdemalion attrs) = runQueueT $ scenarioI18n "theTatterdemalion" $ case msg of
    Setup -> runScenarioSetup TheTatterdemalion attrs do
      gather Set.TheTatterdemalion
      gather Set.Anachronism
      gather Set.DarkPast
      gatherAndSetAside Set.ArtificialIntelligence
      setAgendaDeck [Agendas.emergencyProcedure, Agendas.theGhostShip, Agendas.riseOfTheMachines]
      setActDeck [Acts.eventHorizon, Acts.artificalInsanity, Acts.reconnected]
      setAside [Assets.virtualAccessKey]
      {- "Create the scanning deck. This is done by taking all the encounter
      cards with icons at the bottom of their back side and shuffling them
      together." This runs before the locations are placed so that the
      Ventilation Shaft — a scanning-deck location — is pulled out of the pool
      rather than placed, and so that none of these cards are left behind in the
      encounter deck. -}
      addScanningDeck
      placeAll
        [ Locations.cargoHold
        , Locations.engineRoomTatterdemalion
        , Locations.escapePodBay
        , Locations.infirmaryTatterdemalion
        , Locations.messHall
        , Locations.shipsBridge
        ]
      startAt =<< place Locations.cryosleepQuarters
    -- [cultist]: "If you fail, place 1 of your clues onto your current
    -- location." The engine clamps the amount to the clues actually available,
    -- so no separate "has clues" guard is needed.
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> placeCluesOnLocation iid Cultist 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      resolutionKey <- case r of
        NoResolution -> do
          record YouWereTransportedToTheVirtualDreamlandsByMaja
          pure "noResolution"
        Resolution 1 -> do
          record YouEnteredTheVirtualDreamlandsByYourOwnMeans
          pure "resolution1"
        _ -> error "invalid resolution"

      {- "If an investigator was defeated, resigned, or ended their game with
      Cybervirus in their hand, that investigator must record that they have
      been infected by the cybervirus." -}
      eachInvestigator \iid -> do
        infected <-
          selectAny
            $ enemyIs Enemies.cybervirus
            <> EnemyHiddenInHand (InvestigatorWithId iid)
        when infected $ recordForInvestigator iid HasBeenInfectedByTheCybervirus

      {- "If at least 1 copy of the Reminiscence treachery is in the victory
      display, add 1 [elder thing] token to the chaos bag for the remainder of
      the campaign." -}
      reminiscences <-
        selectAny
          $ VictoryDisplayCardMatch
          $ basic
          $ mapOneOf
            (cardIs . toCardDef)
            [ Treacheries.reminiscencePledge
            , Treacheries.reminiscenceSecrets
            , Treacheries.reminiscenceCovenant
            ]
      when reminiscences $ addChaosToken ElderThing

      earnXp attrs resolutionKey
      endOfScenario
      pure s
    _ -> TheTatterdemalion <$> liftRunMessage msg attrs
