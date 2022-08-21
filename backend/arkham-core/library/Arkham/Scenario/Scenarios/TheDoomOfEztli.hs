module Arkham.Scenario.Scenarios.TheDoomOfEztli
  ( TheDoomOfEztli(..)
  , theDoomOfEztli
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheDoomOfEztli.Story
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype TheDoomOfEztli = TheDoomOfEztli ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomOfEztli :: Difficulty -> TheDoomOfEztli
theDoomOfEztli difficulty = scenario
  TheDoomOfEztli
  "04054"
  "The Doom of Eztli"
  difficulty
  [ ".        ancientHall  undergroundRuins .             ."
  , "entryway ancientHall  undergroundRuins secretPassage chamberOfTime"
  , "entryway grandChamber burialPit        secretPassage chamberOfTime"
  , ".        grandChamber burialPit        .             ."
  ]

instance HasTokenValue TheDoomOfEztli where
  getTokenValue iid tokenFace (TheDoomOfEztli attrs) = case tokenFace of
    Skull -> do
      hasDoom <- selectAny $ LocationWithAnyDoom <> locationWithInvestigator iid
      pure $ if hasDoom
        then toTokenValue attrs Skull 3 4
        else toTokenValue attrs Skull 1 2
    face | face `elem` [Cultist, Tablet] -> do
      n <- if isEasyStandard attrs
        then selectCount LocationWithAnyDoom
        else getSum <$> selectAgg Sum LocationDoom LocationWithAnyDoom
      pure $ TokenValue Cultist (NegativeModifier n)
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog = mkCampaignLog
  { campaignLogRecorded = setFromList
    [TheInvestigatorsClearedAPathToTheEztliRuins]
  }

instance RunMessage TheDoomOfEztli where
  runMessage msg s@(TheDoomOfEztli attrs) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ push $ SetTokens standaloneTokens
      pure s
    StandaloneSetup ->
      pure
        . TheDoomOfEztli
        $ attrs
        & standaloneCampaignLogL
        .~ standaloneCampaignLog
    Setup -> do
      iids <- getInvestigatorIds
      -- | Determine intro
      forcedToWaitForAdditionalSupplies <- getHasRecord
        TheInvestigatorsWereForcedToWaitForAdditionalSupplies
      let intro = if forcedToWaitForAdditionalSupplies then intro1 else intro2
      -- | Setup
      -- -- | Gather cards
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.harbingerOfValusia]
        [ EncounterSet.TheDoomOfEztli
        , EncounterSet.AgentsOfYig
        , EncounterSet.YigsVenom
        , EncounterSet.TemporalFlux
        , EncounterSet.DeadlyTraps
        , EncounterSet.ForgottenRuins
        , EncounterSet.Poison
        , EncounterSet.ChillingCold
        ]

      let
        encounterDeck' = removeEachFromDeck
          encounterDeck
          [ Treacheries.illOmen
          , Treacheries.deepDark
          , Treacheries.finalMistake
          , Treacheries.entombed
          , Treacheries.cryptChill
          ]

      -- Put entryway into play investigators start there
      entryway <- genCard Locations.entryway
      -- | Messages

      explorationDeck <- shuffleM =<< traverse
        genCard
        [ Locations.ancientHall
        , Locations.grandChamber
        , Locations.burialPit
        , Locations.undergroundRuins
        , Locations.secretPassage
        , Treacheries.illOmen
        , Treacheries.deepDark
        , Treacheries.finalMistake
        , Treacheries.entombed
        , Treacheries.cryptChill
        ]

      setAsidePoisonedCount <- getSetAsidePoisonedCount

      setAsideCards <-
        traverse genCard
        $ [ Locations.chamberOfTime
          , Assets.relicOfAgesADeviceOfSomeSort
          , Enemies.harbingerOfValusia
          ]
        <> replicate setAsidePoisonedCount Treacheries.poisoned

      pushAll
        [ story iids intro
        , SetEncounterDeck encounterDeck'
        , SetAgendaDeck
        , SetActDeck
        , PlaceLocation entryway
        , RevealLocation Nothing (toLocationId entryway)
        , MoveAllTo (toSource attrs) (toLocationId entryway)
        ]

      TheDoomOfEztli <$> runMessage
        msg
        (attrs
        & (decksL . at ExplorationDeck ?~ explorationDeck)
        & (setAsideCardsL .~ setAsideCards)
        & (agendaStackL
          . at 1
          ?~ [Agendas.somethingStirs, Agendas.theTempleWarden]
          )
        & (actStackL
          . at 1
          ?~ [Acts.intoTheRuins, Acts.magicAndScience, Acts.escapeTheRuins]
          )
        )
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher
      pure s
    ResolveToken _ ElderThing iid -> do
      when (isHardExpert attrs) $ do
        mlid <- field InvestigatorLocation iid
        for_ mlid $ \lid -> push $ PlaceDoom (LocationTarget lid) 1
      pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> do
      case tokenFace token of
        ElderThing | isEasyStandard attrs -> do
          mlid <- field InvestigatorLocation iid
          for_ mlid $ \lid -> push $ PlaceDoom (LocationTarget lid) 1
        _ -> pure ()
      pure s
    _ -> TheDoomOfEztli <$> runMessage msg attrs
