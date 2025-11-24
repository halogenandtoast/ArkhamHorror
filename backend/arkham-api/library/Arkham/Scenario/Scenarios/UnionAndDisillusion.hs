module Arkham.Scenario.Scenarios.UnionAndDisillusion (setupUnionAndDisillusion, unionAndDisillusion, UnionAndDisillusion (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack.Types
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Scenario qualified as Scenario
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Scenario.Types (setStandaloneCampaignLog)
import Arkham.Scenarios.UnionAndDisillusion.Helpers
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

{- FOURMOLU_DISABLE -}
standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecordedSets =
        mapFromList
          [
            ( toCampaignLogKey MissingPersons
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
    , campaignLogRecorded = setFromList [toCampaignLogKey JosefIsAliveAndWell]
    }

setupUnionAndDisillusion :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupUnionAndDisillusion _attrs = do
  setup $ ul do
    li "gatherSets"
    li "setSetsAside"
    li.nested "placeLocations" do
      li "sidedWithTheCoven"
    li "setAside"
    li "setEnemiesAside"
    li.nested "missingPersons" do
      li "gavriella"
      li "penny"
      li "jerome"
      li "valentino"
      li "removeRemainder"
    li.nested "acts" do
      li "v1"
      li "v2"
      li "v3"
      li "v4"
    li "heretics"
    unscoped $ li "shuffleRemainder"

  whenReturnTo $ gather Set.ReturnToUnionAndDisillusion
  gather Set.UnionAndDisillusion
  gather Set.InexorableFate `orWhenReturnTo` gather Set.UnspeakableFate
  gather Set.RealmOfDeath `orWhenReturnTo` gather Set.UnstableRealm
  gather Set.SpectralPredators
  gather Set.AncientEvils `orWhenReturnTo` gather Set.ImpendingEvils
  gather Set.ChillingCold `orWhenReturnTo` gather Set.ChillingMists

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

  sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
  when sidedWithTheCoven do
    traverse_ lightBrazier (forbiddingShore : unvisitedIsles)

  sidedWithTheLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
  deceivingTheLodge <- getHasRecord TheInvestigatorsAreDeceivingTheLodge
  inductedIntoTheInnerCircle <- getHasRecord TheInvestigatorsWereInductedIntoTheInnerCircle
  hidTheirKnowledge <- getHasRecord TheInvestigatorsHidTheirKnowledgeOfTheCoven
  keptMementosHidden <- getHasRecord TheInvestigatorsKeptsTheirMementosHidden
  erynnJoinedTheInvestigators <- getHasRecord ErynnJoinedTheInvestigators

  let
    (act3, act4)
      | sidedWithTheLodge && erynnJoinedTheInvestigators = (Acts.beyondTheMistV5, Acts.theBindingRite)
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

  hereticCount <- getRecordCount HereticsWereUnleashedUntoArkham
  placeDoomOnAgenda hereticCount

  whenReturnTo $ addAdditionalReferences ["54016b"]

instance RunMessage UnionAndDisillusion where
  runMessage msg s@(UnionAndDisillusion attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "body") do
        labeled' "complete" $ record TheInvestigatorsSidedWithTheLodge
        labeled' "stop" $ record TheInvestigatorsSidedWithTheCoven

      doStep 2 msg
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      erynnJoinedTheInvestigators <- getHasRecord ErynnJoinedTheInvestigators
      sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      when (sidedWithTheCoven && erynnJoinedTheInvestigators) do
        flavor $ setTitle "title" >> p "additionalIntro"
      pure s
    StandaloneSetup -> do
      push (SetChaosTokens standaloneChaosTokens)
      pure $ overAttrs (setStandaloneCampaignLog standaloneCampaignLog) s
    Setup -> runScenarioSetup UnionAndDisillusion attrs $ setupUnionAndDisillusion attrs
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
      chooseTargetM iid enemies \enemy ->
        initiateEnemyAttackEdit enemy Tablet iid \atk -> atk {attackDespiteExhausted = True}
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderThing)) _ _ -> do
      mAction <- getSkillTestAction
      when (mAction == Just Circle) $ runHauntedAbilities iid
      pure s
    ScenarioResolution n -> scope "resolutions" do
      case n of
        NoResolution -> do
          resolution "noResolution"
          push R5
        Resolution 1 -> do
          inductedIntoTheInnerCircle <- getHasRecord TheInvestigatorsWereInductedIntoTheInnerCircle
          deceivingTheLodge <- getHasRecord TheInvestigatorsAreDeceivingTheLodge

          storyWithChooseOneM' (compose.resolution $ setTitle "resolution1.title" >> p "resolution1.body") $ unscoped do
            labeledValidate' (inductedIntoTheInnerCircle && not deceivingTheLodge) "yes" $ push R2
            labeled' "no" $ push R3
        Resolution 2 -> do
          resolution "resolution2"
          record TheTrueWorkOfTheSilverTwilightLodgeHasBegun
          gameOver
        Resolution 3 -> do
          resolution "resolution3"
          record CarlSanfordPossessesTheSecretsOfTheUniverse
          push R8
        Resolution 4 -> do
          isReturnTo <- Scenario.getIsReturnTo
          if isReturnTo
            then do
              erynnJoinedTheInvestigators <- getHasRecord ErynnJoinedTheInvestigators
              hasBlackBook <- isJust <$> getOwner Assets.theBlackBook
              storyWithChooseOneM'
                (compose.resolution $ setTitle "returnToResolution4.title" >> p "returnToResolution4.body")
                do
                  labeledValidate' (erynnJoinedTheInvestigators && hasBlackBook) "accept" $ push R9
                  labeled' "flee" $ push R10
            else do
              resolution "resolution4"
              record AnetteMasonIsPossessedByEvil
              push R8
        Resolution 5 -> do
          -- Right column is easier to check so we use that one
          spellCast <- getHasRecord TheWitches'SpellWasCast
          josefDisappearedIntoTheMist <- getHasRecord JosefDisappearedIntoTheMist
          hereticsUnleashed <- getRecordCount HereticsWereUnleashedUntoArkham
          let total = count id [not spellCast, not josefDisappearedIntoTheMist, hereticsUnleashed <= 1]

          resolutionFlavor $ scope "resolution5" do
            setTitle "title"
            p "body"
            cols do
              ul do
                li.validate (not spellCast) "spellBroken"
                li.validate (not josefDisappearedIntoTheMist) "rescued"
                li.validate (hereticsUnleashed <= 1) "fewerHeretics"
              ul do
                li.validate spellCast "spellCast"
                li.validate josefDisappearedIntoTheMist "disappeared"
                li.validate (hereticsUnleashed >= 2) "moreHeretics"
            p "continue"
          push $ if total >= 2 then R6 else R7
        Resolution 6 -> do
          resolution "resolution6"
          record CarlSanfordPossessesTheSecretsOfTheUniverse
          push R8
        Resolution 7 -> do
          resolution "resolution7"
          record AnetteMasonIsPossessedByEvil
          push R8
        Resolution 8 -> do
          resolutionWithXp "resolution8" $ allGainXp' attrs
          removeCampaignCard Assets.puzzleBox
          investigators <- allInvestigators

          gavriellaIsAlive <- getHasRecord GavriellaIsAlive
          if gavriellaIsAlive
            then addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.gavriellaMizrah
            else record GavriellaIsDead

          jeromeIsAlive <- getHasRecord JeromeIsAlive
          if jeromeIsAlive
            then addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.jeromeDavids
            else record JeromeIsDead

          pennyIsAlive <- getHasRecord PennyIsAlive
          if pennyIsAlive
            then addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.pennyWhite
            else record PennyIsDead

          valentinoIsAlive <- getHasRecord ValentinoIsAlive
          if valentinoIsAlive
            then addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.valentinoRivas
            else record ValentinoIsDead
          endOfScenario
        Resolution 9 -> do
          resolution "resolution9"
          record TheCovenOfKeziahHoldsTheWorldInItsGrasp
          gameOver
        Resolution 10 -> do
          resolution "resolution10"
          record AnetteMasonIsPossessedByEvil
          push R8
        _ -> error "Invalid resolution"
      pure s
    _ -> UnionAndDisillusion <$> liftRunMessage msg attrs
