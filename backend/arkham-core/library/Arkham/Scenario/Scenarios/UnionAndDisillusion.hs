module Arkham.Scenario.Scenarios.UnionAndDisillusion (
  UnionAndDisillusion (..),
  unionAndDisillusion,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Field
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message hiding (InvestigatorDamage)
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.UnionAndDisillusion.Story
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Spectral))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Zone

newtype UnionAndDisillusion = UnionAndDisillusion ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unionAndDisillusion :: Difficulty -> UnionAndDisillusion
unionAndDisillusion difficulty =
  scenario
    UnionAndDisillusion
    "05238"
    "Union and Disillusion"
    difficulty
    [ ".              miskatonicRiver ."
    , ".              forbiddingShore ."
    , "unvisitedIsle1 .               unvisitedIsle2"
    , ".              theGeistTrap    ."
    ]

instance HasChaosTokenValue UnionAndDisillusion where
  getChaosTokenValue iid chaosTokenFace (UnionAndDisillusion attrs) = case chaosTokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecordedSets =
        mapFromList
          [
            ( MissingPersons
            , [ crossedOut (toCardCode i)
              | i <-
                  [ Investigators.gavriellaMizrah
                  , Investigators.jeromeDavids
                  , Investigators.valentinoRivas
                  , Investigators.pennyWhite
                  ]
              ]
            )
          ]
    , campaignLogRecorded = setFromList [JosefIsAliveAndWell]
    }

instance RunMessage UnionAndDisillusion where
  runMessage msg s@(UnionAndDisillusion attrs) = case msg of
    SetChaosTokensForScenario -> do
      pushWhenM getIsStandalone (SetChaosTokens standaloneChaosTokens)
      pure s
    PreScenarioSetup -> do
      investigators <- allInvestigators
      lead <- getLead
      pushAll
        [ story investigators intro
        , questionLabel
            "This is a point of no return—you will not get the chance to change your mind later. The investigators must decide (choose one):"
            lead
            $ ChooseOne
              [ Label
                  "\"We have to help complete the Lodge’s ritual.\" Completing the ritual should bind the Spectral Watcher and prevent it from doing any more harm."
                  [Record TheInvestigatorsSidedWithTheLodge]
              , Label
                  "\"We have to stop the Lodge’s ritual.\" Disrupting the ritual should release the Spectral Watcher’s tether to the mortal realm."
                  [Record TheInvestigatorsSidedWithTheCoven]
              ]
        ]
      pure s
    StandaloneSetup -> do
      pure
        . UnionAndDisillusion
        $ attrs
          & standaloneCampaignLogL
            .~ standaloneCampaignLog
    Setup -> do
      encounterDeck <-
        buildEncounterDeckExcluding
          [Treacheries.watchersGaze]
          [ EncounterSet.UnionAndDisillusion
          , EncounterSet.InexorableFate
          , EncounterSet.RealmOfDeath
          , EncounterSet.SpectralPredators
          , EncounterSet.AncientEvils
          , EncounterSet.ChillingCold
          ]

      missingPersons <- getRecordedCardCodes MissingPersons

      setAsideCards <-
        liftA2
          (<>)
          ( concatMapM
              (fmap (map toCard) . gatherEncounterSet)
              [EncounterSet.AnettesCoven, EncounterSet.SilverTwilightLodge, EncounterSet.TheWatcher]
          )
          ( genCards $
              [Locations.theGeistTrap, Treacheries.watchersGaze, Enemies.anetteMason, Enemies.josefMeiger]
                <> [Assets.gavriellaMizrah | "05046" `elem` missingPersons]
                <> [Assets.jeromeDavids | "05047" `elem` missingPersons]
                <> [Assets.valentinoRivas | "05048" `elem` missingPersons]
                <> [Assets.pennyWhite | "05049" `elem` missingPersons]
          )

      storyCards <-
        genCards $
          [Stories.gavriellasFate | "05046" `elem` missingPersons]
            <> [Stories.jeromesFate | "05047" `elem` missingPersons]
            <> [Stories.valentinosFate | "05048" `elem` missingPersons]
            <> [Stories.pennysFate | "05049" `elem` missingPersons]

      sidedWithTheLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
      sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      deceivingTheLodge <- getHasRecord TheInvestigatorsAreDeceivingTheLodge
      inductedIntoTheInnerCircle <- getHasRecord TheInvestigatorsWereInductedIntoTheInnerCircle
      hidTheirKnowledge <- getHasRecord TheInvestigatorsHidTheirKnowledgeOfTheCoven
      keptMementosHidden <- getHasRecord TheInvestigatorsKeptsTheirMementosHidden
      hereticCount <- getRecordCount HereticsWereUnleashedUntoArkham

      unvisitedIsleCards <-
        sampleN
          2
          $ Locations.unvisitedIsleStandingStones
            :| [ Locations.unvisitedIsleMistyClearing
               , Locations.unvisitedIsleForsakenWoods
               , Locations.unvisitedIsleMossCoveredSteps
               , Locations.unvisitedIsleHauntedSpring
               , Locations.unvisitedIsleDecayedWillow
               ]

      (miskatonicRiver, placeMiskatonicRiver) <- placeLocationCard Locations.miskatonicRiver
      (forbiddingShore, placeForbiddingShore) <- placeLocationCard Locations.forbiddingShore
      (unvisitedIsles, placeUnvisitedIsles) <- placeLabeledLocations "unvisitedIsle" unvisitedIsleCards

      let lightBrazier location = UpdateLocation location (LocationBrazier ?=. Lit)

      theWatcher <- genCard Enemies.theSpectralWatcher

      placeTheWatcher <- createEnemyWithPlacement_ theWatcher (OutOfPlay SetAsideZone)

      pushAll $
        [SetEncounterDeck encounterDeck, SetAgendaDeck, SetActDeck]
          <> replicate hereticCount PlaceDoomOnAgenda
          <> [placeMiskatonicRiver, MoveAllTo (toSource attrs) miskatonicRiver, placeForbiddingShore]
          <> placeUnvisitedIsles
          <> (if sidedWithTheCoven then map lightBrazier (forbiddingShore : unvisitedIsles) else [])
          <> [placeTheWatcher]

      let
        (act3, act4)
          | sidedWithTheLodge = (Acts.beyondTheMistV1, Acts.theBindingRite)
          | sidedWithTheCoven
          , deceivingTheLodge
          , inductedIntoTheInnerCircle =
              (Acts.beyondTheMistV2, Acts.theBrokenRite)
          | sidedWithTheCoven
          , count id [deceivingTheLodge, hidTheirKnowledge, keptMementosHidden] >= 2 =
              (Acts.beyondTheMistV3, Acts.theBrokenRite)
          | otherwise = (Acts.beyondTheMistV4, Acts.theBrokenRite)

      agendas <- genCards [Agendas.theLoversVI, Agendas.crossroadsOfFate]
      acts <- genCards [Acts.theUnvisitedIsle, Acts.fatedSouls, act3, act4]

      UnionAndDisillusion
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ filter (`cardMatch` NotCard (cardIs Enemies.theSpectralWatcher)) setAsideCards)
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
              & (cardsUnderScenarioReferenceL .~ storyCards)
          )
    ResolveChaosToken _ Skull iid -> do
      mAction <- getSkillTestAction
      when (mAction == Just Circle) $ do
        push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Cultist iid -> do
      damage <- field InvestigatorDamage iid
      horror <- field InvestigatorHorror iid
      when (damage == 0 || horror == 0) $ do
        push $
          InvestigatorAssignDamage
            iid
            (ChaosTokenEffectSource Cultist)
            DamageAny
            (if damage == 0 then 1 else 0)
            (if horror == 0 then 1 else 0)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      enemies <- selectList $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Spectral
      unless (null enemies) $ do
        push $
          chooseOrRunOne
            iid
            [ targetLabel enemy [InitiateEnemyAttack $ enemyAttack enemy (ChaosTokenEffectSource Tablet) iid]
            | enemy <- enemies
            ]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderThing)) _ _ -> do
      mAction <- getSkillTestAction
      when (mAction == Just Circle) $ runHauntedAbilities iid
      pure s
    _ -> UnionAndDisillusion <$> runMessage msg attrs
