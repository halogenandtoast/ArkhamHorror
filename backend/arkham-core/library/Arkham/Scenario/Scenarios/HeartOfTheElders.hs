module Arkham.Scenario.Scenarios.HeartOfTheElders
  ( HeartOfTheElders(..)
  , heartOfTheElders
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.HeartOfTheElders.Story
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait ( Trait (Cave) )
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

data ScenarioStep = One | Two
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Metadata = Metadata
  { scenarioStep :: ScenarioStep
  , reachedAct2 :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HeartOfTheElders = HeartOfTheElders (ScenarioAttrs `With` Metadata)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heartOfTheElders :: Difficulty -> HeartOfTheElders
heartOfTheElders difficulty = scenario
  (HeartOfTheElders . (`with` Metadata One False))
  "04205"
  "Heart of the Elders"
  difficulty
  [ ".               .               timeWrackedWoods timeWrackedWoods .               ."
  , ".               .               timeWrackedWoods timeWrackedWoods .               ."
  , "pathOfThorns    pathOfThorns    riverCanyon      riverCanyon      ropeBridge      ropeBridge"
  , "pathOfThorns    pathOfThorns    riverCanyon      riverCanyon      ropeBridge      ropeBridge"
  , ".               serpentsHaven   serpentsHaven    circuitousTrail  circuitousTrail ."
  , ".               serpentsHaven   serpentsHaven    circuitousTrail  circuitousTrail ."
  , "templeOfTheFang templeOfTheFang stoneAltar       stoneAltar       overgrownRuins overgrownRuins"
  , "templeOfTheFang templeOfTheFang stoneAltar       stoneAltar       overgrownRuins overgrownRuins"
  , ".               .               mouthOfKnYan     mouthOfKnYan     .              ."
  , ".               .               mouthOfKnYan     mouthOfKnYan     .              ."
  ]

part2Locations :: [GridTemplateRow]
part2Locations =
  [ ".            perilousGulch .              ."
  , ".            perilousGulch crystalPillars ."
  , "mouthOfKnYan vastPassages  crystalPillars descentToYoth"
  , "mouthOfKnYan vastPassages  hallOfIdolatry descentToYoth"
  , ".            darkHollow    hallOfIdolatry ."
  , ".            darkHollow    .              ."
  ]

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
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance HasTokenValue HeartOfTheElders where
  getTokenValue iid tokenFace (HeartOfTheElders (attrs `With` _)) =
    case tokenFace of
      Skull -> do
        inCave <-
          selectAny $ locationWithInvestigator iid <> LocationWithTrait Cave
        let caveModifier = if inCave then 2 else 0
        pure $ toTokenValue attrs Skull (1 + caveModifier) (2 + caveModifier)
      Cultist -> pure $ toTokenValue attrs Cultist 2 3
      Tablet -> pure $ toTokenValue attrs Tablet 2 3
      ElderThing -> pure $ toTokenValue attrs ElderThing 3 4
      otherFace -> getTokenValue iid otherFace attrs

instance RunMessage HeartOfTheElders where
  runMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ push $ SetTokens standaloneTokens
      pure s
    StandaloneSetup -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push
        $ Ask leadInvestigatorId
        $ QuestionLabel
            "The investigators may choose how many paths are known to you (choose a number between 0 and 5). The more paths are known to you, the quicker and easier the scenario will be."
        $ ChooseOne
            [ Label (tshow n) [RecordCount PathsAreKnownToYou n]
            | n <- [0 .. 5]
            ]
      pure s
    Setup -> do
      iids <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId

      mIchtacaInvestigator <- getOwner Assets.ichtacaTheForgottenGuardian
      mAlejandroInvestigator <- getOwner Assets.alejandroVela
      mExpeditionJournalInvestigator <- getOwner Assets.expeditionJournal


      let
        introMessages =
          [ story iids intro1
          , chooseOrRunOne leadInvestigatorId
            $ [ Label
                  "Let’s consult with Ichtaca."
                  [ story iids intro2
                  , PutCampaignCardIntoPlay
                    iid
                    Assets.ichtacaTheForgottenGuardian
                  ]
              | iid <- maybeToList mIchtacaInvestigator
              ]
            <> [ Label
                   "Let’s consult with Alejandro."
                   [ story iids intro3
                   , PutCampaignCardIntoPlay iid Assets.alejandroVela
                   ]
               | iid <- maybeToList mAlejandroInvestigator
               ]
            <> [ Label
                   "Let’s consult the expedition journal."
                   [ story iids intro4
                   , PutCampaignCardIntoPlay iid Assets.expeditionJournal
                   ]
               | iid <- maybeToList mExpeditionJournalInvestigator
               ]
            <> [Label "I wish we knew more about this..." []]
          ]

      pathsKnown <- getRecordCount PathsAreKnownToYou

      if pathsKnown == 6
        then do
          pushAll $ introMessages <> [ScenarioResolution $ Resolution 1]
          pure s
        else do
          let
            explorationDeckLocations =
              [ Locations.timeWrackedWoods
              , Locations.pathOfThorns
              , Locations.riverCanyon
              , Locations.ropeBridge
              , Locations.serpentsHaven
              , Locations.circuitousTrail
              ]
            ruinsLocations =
              Locations.overgrownRuins
                :| [Locations.templeOfTheFang, Locations.stoneAltar]
          encounterDeck' <-
            buildEncounterDeckExcluding
                (explorationDeckLocations <> toList ruinsLocations)
              $ [ EncounterSet.PillarsOfJudgement
                , EncounterSet.HeartOfTheElders
                , EncounterSet.Rainforest
                , EncounterSet.Serpents
                , EncounterSet.Expedition
                , EncounterSet.Poison
                ]

          let
            encounterDeck = removeEachFromDeck
              encounterDeck'
              [ Treacheries.pitfall
              , Treacheries.ants
              , Treacheries.lostInTheWilds
              , Treacheries.lowOnSupplies
              ]

          mouthOfKnYanTheCavernsMaw <- genCard
            Locations.mouthOfKnYanTheCavernsMaw

          theWingedSerpent <- genCard Enemies.theWingedSerpent

          ruinsLocation <- genCard =<< sample ruinsLocations
          mappedOutTheWayForward <- getHasRecord
            TheInvestigatorsMappedOutTheWayForward

          explorationDeck <-
            shuffleM
            . (<> [ ruinsLocation | not mappedOutTheWayForward ])
            =<< traverse
                  genCard
                  (explorationDeckLocations
                  <> [ Treacheries.pitfall
                     , Treacheries.ants
                     , Treacheries.lostInTheWilds
                     , Treacheries.lowOnSupplies
                     ]
                  )

          setAsidePoisonedCount <- getSetAsidePoisonedCount

          setAsideCards <- traverse
            genCard
            (replicate setAsidePoisonedCount Treacheries.poisoned)

          pushAll
            $ introMessages
            <> [ SetEncounterDeck encounterDeck
               , SetAgendaDeck
               , SetActDeck
               , PlaceLocation mouthOfKnYanTheCavernsMaw
               , MoveAllTo
                 (toSource attrs)
                 (toLocationId mouthOfKnYanTheCavernsMaw)
               , PlaceResources
                 (LocationTarget $ toLocationId mouthOfKnYanTheCavernsMaw)
                 pathsKnown
               ]
            <> [ CreateEnemyAt
                   theWingedSerpent
                   (toLocationId mouthOfKnYanTheCavernsMaw)
                   Nothing
               | reachedAct2 metadata
               ]
            <> [ PlaceLocation ruinsLocation | mappedOutTheWayForward ]

          HeartOfTheElders . (`with` metadata) <$> runMessage
            msg
            (attrs
            & (decksL . at ExplorationDeck ?~ explorationDeck)
            & (setAsideCardsL .~ setAsideCards)
            & (agendaStackL
              . at 1
              ?~ [Agendas.theJunglesHeart, Agendas.settingSun]
              )
            & (actStackL
              . at 1
              ?~ [Acts.searchForThePattern, Acts.openingTheMaw]
              )
            )
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored
      pure s
    _ -> HeartOfTheElders . (`with` metadata) <$> runMessage msg attrs
