module Arkham.Scenario.Scenarios.UnionAndDisillusion (UnionAndDisillusion (..), unionAndDisillusion) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice)
import Arkham.Scenario.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Scenario.Types (setStandaloneCampaignLog)
import Arkham.Scenarios.UnionAndDisillusion.Helpers
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
    , "unvisitedIsle3 forbiddingShore unvisitedIsle4"
    , "unvisitedIsle1 .               unvisitedIsle2"
    , "unvisitedIsle5 theGeistTrap    unvisitedIsle6"
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
  runMessage msg s@(UnionAndDisillusion attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro

      lead <- getLead
      chooseOneM lead do
        questionLabeled
          "This is a point of no return—you will not get the chance to change your mind later. The investigators must decide (choose one):"
        labeled
          "\"We have to help complete the Lodge’s ritual.\" Completing the ritual should bind the Spectral Watcher and prevent it from doing any more harm."
          $ record TheInvestigatorsSidedWithTheLodge
        labeled
          "\"We have to stop the Lodge’s ritual.\" Disrupting the ritual should release the Spectral Watcher’s tether to the mortal realm."
          $ record TheInvestigatorsSidedWithTheCoven
      pure s
    StandaloneSetup -> do
      push (SetChaosTokens standaloneChaosTokens)
      pure $ overAttrs (setStandaloneCampaignLog standaloneCampaignLog) s
    Setup -> runScenarioSetup UnionAndDisillusion attrs do
      gather Set.UnionAndDisillusion
      gather Set.InexorableFate
      gather Set.RealmOfDeath
      gather Set.SpectralPredators
      gather Set.AncientEvils
      gather Set.ChillingCold

      gatherAndSetAside Set.AnettesCoven
      gatherAndSetAside Set.SilverTwilightLodge
      gatherAndSetAside Set.TheWatcher -- need to remove the spectral watcher
      missingPersons <- getRecordedCardCodes MissingPersons
      (unvisitedIsleCards, unplaceUnvisitedIsles) <-
        splitAt 2
          <$> shuffleM
            [ Locations.unvisitedIsleStandingStones
            , Locations.unvisitedIsleMistyClearing
            , Locations.unvisitedIsleForsakenWoods
            , Locations.unvisitedIsleMossCoveredSteps
            , Locations.unvisitedIsleHauntedSpring
            , Locations.unvisitedIsleDecayedWillow
            ]
      setAside
        $ [ Locations.theGeistTrap
          , Treacheries.watchersGazeUnionAndDisillusion
          , Enemies.anetteMason
          , Enemies.josefMeiger
          ]
        <> [Assets.gavriellaMizrah | "05046" `elem` missingPersons]
        <> [Assets.jeromeDavids | "05047" `elem` missingPersons]
        <> [Assets.valentinoRivas | "05048" `elem` missingPersons]
        <> [Assets.pennyWhite | "05049" `elem` missingPersons]
        <> unplaceUnvisitedIsles

      placeUnderScenarioReference
        $ [Stories.gavriellasFate | "05046" `elem` missingPersons]
        <> [Stories.jeromesFate | "05047" `elem` missingPersons]
        <> [Stories.valentinosFate | "05048" `elem` missingPersons]
        <> [Stories.pennysFate | "05049" `elem` missingPersons]

      startAt =<< place Locations.miskatonicRiver
      forbiddingShore <- place Locations.forbiddingShore
      unvisitedIsles <- placeGroupCapture "unvisitedIsle" unvisitedIsleCards

      placeEnemy Enemies.theSpectralWatcher (OutOfPlay SetAsideZone)

      hereticCount <- getRecordCount HereticsWereUnleashedUntoArkham
      placeDoomOnAgenda hereticCount

      sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      when sidedWithTheCoven
        $ traverse_ (push . lightBrazier) (forbiddingShore : unvisitedIsles)

      sidedWithTheLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
      deceivingTheLodge <- getHasRecord TheInvestigatorsAreDeceivingTheLodge
      inductedIntoTheInnerCircle <- getHasRecord TheInvestigatorsWereInductedIntoTheInnerCircle
      hidTheirKnowledge <- getHasRecord TheInvestigatorsHidTheirKnowledgeOfTheCoven
      keptMementosHidden <- getHasRecord TheInvestigatorsKeptsTheirMementosHidden

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

      setAgendaDeck [Agendas.theLoversVI, Agendas.crossroadsOfFate]
      setActDeck [Acts.theUnvisitedIsle, Acts.fatedSouls, act3, act4]
    ResolveChaosToken _ Skull iid -> do
      mAction <- getSkillTestAction
      when (mAction == Just Circle) $ drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Cultist iid -> do
      damage <- field InvestigatorDamage iid
      horror <- field InvestigatorHorror iid
      when (damage == 0 || horror == 0) $ do
        assignDamageAndHorror iid Cultist (if damage == 0 then 1 else 0) (if horror == 0 then 1 else 0)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      enemies <- select $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Spectral
      chooseTargetM iid enemies \enemy -> initiateEnemyAttack enemy Tablet iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderThing)) _ _ -> do
      mAction <- getSkillTestAction
      when (mAction == Just Circle) $ runHauntedAbilities iid
      pure s
    ScenarioResolution n -> do
      case n of
        NoResolution -> do
          story noResolution
          push R5
        Resolution 1 -> do
          inductedIntoTheInnerCircle <- getHasRecord TheInvestigatorsWereInductedIntoTheInnerCircle
          deceivingTheLodge <- getHasRecord TheInvestigatorsAreDeceivingTheLodge

          storyWithChooseOneM resolution1 do
            when (inductedIntoTheInnerCircle && not deceivingTheLodge) do
              labeled "Yes" $ push R2
            labeled "No" $ push R3
        Resolution 2 -> do
          story resolution2
          record TheTrueWorkOfTheSilverTwilightLodgeHasBegun
          gameOver
        Resolution 3 -> do
          story resolution3
          record CarlSanfordPossessesTheSecretsOfTheUniverse
          push R8
        Resolution 4 -> do
          story resolution4
          record AnetteMasonIsPossessedByEvil
          push R8
        Resolution 5 -> do
          -- Right column is easier to check so we use that one
          spellBroken <- getHasRecord TheWitches'SpellWasCast
          josefDisappearedIntoTheMist <- getHasRecord JosefDisappearedIntoTheMist
          hereticsUnleashed <- getRecordCount HereticsWereUnleashedUntoArkham
          let total = count id [spellBroken, josefDisappearedIntoTheMist, hereticsUnleashed >= 2]
          push $ if total >= 2 then R7 else R6
        Resolution 6 -> do
          story resolution6
          record CarlSanfordPossessesTheSecretsOfTheUniverse
          push R8
        Resolution 7 -> do
          story resolution7
          record AnetteMasonIsPossessedByEvil
          push R8
        Resolution 8 -> do
          story resolution8
          removeCampaignCard Assets.puzzleBox
          investigators <- allInvestigators

          gavriellaIsAlive <- getHasRecord GavriellaIsAlive
          if gavriellaIsAlive
            then addCampaignCardToDeckChoice investigators Assets.gavriellaMizrah
            else record GavriellaIsDead

          jeromeIsAlive <- getHasRecord JeromeIsAlive
          if jeromeIsAlive
            then addCampaignCardToDeckChoice investigators Assets.jeromeDavids
            else record JeromeIsDead

          pennyIsAlive <- getHasRecord PennyIsAlive
          if pennyIsAlive
            then addCampaignCardToDeckChoice investigators Assets.pennyWhite
            else record PennyIsDead

          valentinoIsAlive <- getHasRecord ValentinoIsAlive
          if valentinoIsAlive
            then addCampaignCardToDeckChoice investigators Assets.valentinoRivas
            else record ValentinoIsDead
          allGainXp attrs
          endOfScenario
        _ -> error "Invalid resolution"
      pure s
    _ -> UnionAndDisillusion <$> liftRunMessage msg attrs
