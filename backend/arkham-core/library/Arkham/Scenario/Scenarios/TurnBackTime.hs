module Arkham.Scenario.Scenarios.TurnBackTime
  ( TurnBackTime(..)
  , turnBackTime
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
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TurnBackTime.Story
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype TurnBackTime = TurnBackTime ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

turnBackTime :: Difficulty -> TurnBackTime
turnBackTime difficulty = scenario
  TurnBackTime
  "04344"
  "Turn Back Time"
  difficulty
  [ ".        ancientHall  undergroundRuins .             .             .    ."
  , "entryway ancientHall  undergroundRuins secretPassage chamberOfTime .    ."
  , "entryway grandChamber burialPit        secretPassage chamberOfTime .    ."
  , ".        grandChamber burialPit        .             .             .    ."
  , "pos1     pos2         pos3             pos4          pos5          pos6 pos7"
  ]

instance HasTokenValue TurnBackTime where
  getTokenValue iid tokenFace (TurnBackTime attrs) = case tokenFace of
    Skull -> do
      locationsWithDoom <- selectCount LocationWithAnyDoom
      totalLocationDoom <-
        getSum <$> selectAgg Sum LocationDoom LocationWithAnyDoom
      pure $ toTokenValue attrs Skull locationsWithDoom totalLocationDoom
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ toTokenValue attrs ElderThing 4 6
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TurnBackTime where
  runMessage msg s@(TurnBackTime attrs) = case msg of
    CheckWindow _ [Window Timing.When (Window.DrawingStartingHand iid)] -> do
      mRepossessThePast <-
        selectOne $ InDeckOf (InvestigatorWithId iid) <> BasicCardMatch
          (cardIs Assets.relicOfAgesRepossessThePast)
      pushAll
        [ RemovePlayerCardFromGame repossessThePast
        | repossessThePast <- maybeToList mRepossessThePast
        ]
      pure s
    Setup -> do
      forcedToWaitForSupplies <- getHasRecord
        TheInvestigatorsWereForcedToWaitForAdditionalSupplies
      let intro = if forcedToWaitForSupplies then intro1 else intro2
      iids <- allInvestigatorIds
      crossOut <- drop 3 . campaignLogOrderedKeys <$> getCampaignLog

      encounterDeck <- buildEncounterDeckExcluding
        [ toCardDef Enemies.harbingerOfValusia
        , toCardDef Locations.ancientHall
        , toCardDef Locations.grandChamber
        , toCardDef Locations.undergroundRuins
        , toCardDef Locations.burialPit
        , toCardDef Locations.secretPassage
        , toCardDef Locations.chamberOfTime
        ]
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
          [ toCardDef Treacheries.illOmen
          , toCardDef Treacheries.deepDark
          , toCardDef Treacheries.finalMistake
          , toCardDef Treacheries.entombed
          , toCardDef Treacheries.cryptChill
          ]

      explorationDeck <- shuffleM =<< traverse
        genCard
        [ toCardDef Locations.ancientHall
        , toCardDef Locations.grandChamber
        , toCardDef Locations.burialPit
        , toCardDef Locations.undergroundRuins
        , toCardDef Locations.secretPassage
        , toCardDef Treacheries.illOmen
        , toCardDef Treacheries.deepDark
        , toCardDef Treacheries.finalMistake
        , toCardDef Treacheries.entombed
        , toCardDef Treacheries.cryptChill
        ]

      setAsidePoisonedCount <- getSetAsidePoisonedCount

      setAsideCards <-
        traverse genCard
        $ [ toCardDef Locations.chamberOfTime
          , toCardDef Assets.relicOfAgesRepossessThePast
          , toCardDef Enemies.harbingerOfValusia
          ]
        <> replicate setAsidePoisonedCount (toCardDef Treacheries.poisoned)

      (entrywayId, placeEntryway) <- placeLocationCard Locations.entryway

      pushAll
        $ story iids intro
        : map CrossOutRecord crossOut
        <> [ SetEncounterDeck encounterDeck'
           , SetAgendaDeck
           , SetActDeck
           , placeEntryway
           , RevealLocation Nothing entrywayId
           , MoveAllTo (toSource attrs) entrywayId
           , AddToken ElderThing
           ]
      TurnBackTime <$> runMessage
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
          ?~ [ Acts.intoTheRuinsOnceAgain
             , Acts.theChamberOfStillRemains
             , Acts.momentOfDoom
             ]
          )
        )
    ResolveToken _ ElderThing iid | isHardExpert attrs -> do
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
    ScenarioResolution resolution -> do
      iids <- allInvestigatorIds
      case resolution of
        NoResolution ->
          pushAll
            $ Record TheFabricOfTimeIsUnwoven
            : map DrivenInsane iids
            <> [GameOver]
        Resolution 1 -> do
          gainXp <- map (uncurry GainXP) <$> getXp
          pushAll
            $ Record TheInvestigatorsSealedTheRelicOfAgesForever
            : gainXp
            <> [GameOver]
        _ -> error "Unknown Resolution"
      pure s
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    _ -> TurnBackTime <$> runMessage msg attrs
