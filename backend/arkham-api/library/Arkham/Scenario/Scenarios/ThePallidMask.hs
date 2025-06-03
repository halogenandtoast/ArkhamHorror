module Arkham.Scenario.Scenarios.ThePallidMask (setupThePallidMask, thePallidMask, ThePallidMask (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.Card
import Arkham.Distance
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.SkillTest
import Arkham.Token
import Arkham.Trait (Trait (Geist, Ghoul, Madness, Pact))

newtype ThePallidMask = ThePallidMask ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePallidMask :: Difficulty -> ThePallidMask
thePallidMask difficulty = scenario ThePallidMask "03240" "The Pallid Mask" difficulty []

instance HasModifiersFor ThePallidMask where
  getModifiersFor (ThePallidMask a) = do
    modifySelectMaybe a Anyone \iid -> do
      liftGuardM $ elem (recorded $ unInvestigatorId iid) <$> getRecordSet ReadActII
      pure [XPModifier "Read Act II" 2]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecorded =
        setFromList
          $ map toCampaignLogKey [YouFoundNigelsHome, YouEnteredTheCatacombsOnYourOwn]
    }

instance HasChaosTokenValue ThePallidMask where
  getChaosTokenValue iid chaosTokenFace (ThePallidMask attrs) = case chaosTokenFace of
    Skull ->
      field InvestigatorLocation iid >>= \case
        Nothing -> pure $ toChaosTokenValue attrs Skull 0 0
        Just yourLocation -> do
          -- -X where X is the number of locations away from the starting location
          startingLocation <- selectJust $ LocationInPosition (Pos 0 0)
          distance <- unDistance . fromMaybe (Distance 0) <$> getDistance startingLocation yourLocation
          pure $ toChaosTokenValue attrs Skull (min 5 distance) distance
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

setupThePallidMask :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupThePallidMask attrs = do
  setUsesGrid
  setup do
    ul do
      li "gatherSets"
      li "setAside"
      li.nested "start.instructions" do
        li "start.awoke"
        li "start.entered"
      li.nested "catacombs.instructions" do
        li "catacombs.bottom"
        li "catacombs.top"
      li "startAt"
      li "theManInThePallidMask"
      unscoped $ li "shuffleRemainder"

  whenReturnTo $ gather Set.ReturnToThePallidMask
  gather Set.ThePallidMask
  gather Set.Ghouls
  gather Set.Hauntings
  gather Set.ChillingCold

  isReturnTo <- getIsReturnTo

  let
    otherCatacombs =
      [ Locations.stoneArchways
      , Locations.stoneArchways
      , Locations.cryptOfTheSepulchralLamp
      , Locations.boneFilledCaverns
      , Locations.wellOfSouls
      , Locations.candlelitTunnels
      , Locations.candlelitTunnels
      , Locations.labyrinthOfBones
      , Locations.labyrinthOfBones
      , Locations.narrowShaft
      , Locations.shiveringPools
      ]
        <> ( guard isReturnTo
               *> [ Locations.returnToSecretPassage
                  , Locations.moundOfBones
                  , Locations.researchSite
                  , Locations.seaOfSkulls
                  ]
           )

  didNotEscapeGazeOfThePhantom <- getHasRecord YouDidNotEscapeTheGazeOfThePhantom
  unableToFindNigel <- getHasRecord YouWereUnableToFindNigel
  let awokeInsideTheCatacombs = didNotEscapeGazeOfThePhantom || unableToFindNigel
  (start, remainingCatacombs) <-
    if awokeInsideTheCatacombs
      then do
        let
          validStart = \case
            (x : _) -> x /= Locations.researchSite
            _ -> True
          
        shuffled <- retryUntil validStart $ shuffle (Locations.theGateToHell : otherCatacombs)
        case shuffled of
          (x : xs) -> (,xs) <$> placeInGrid (Pos 0 0) x
          _ -> error "invalid setup"
      else (,) <$> placeInGrid (Pos 0 0) Locations.theGateToHell <*> shuffleM otherCatacombs

  startAt start
  placeTokens attrs start Resource 1

  theManInThePallidMask <- getCampaignStoryCard Enemies.theManInThePallidMask
  push $ RemoveFromBearersDeckOrDiscard theManInThePallidMask
  setAside [Enemies.theManInThePallidMask]

  let (bottom3, rest) = splitAt 3 $ if isReturnTo then drop 4 remainingCatacombs else remainingCatacombs
  bottom <- shuffleM ([Locations.tombOfShadows, Locations.blockedPassage] <> bottom3)
  addExtraDeck CatacombsDeck =<< genCards (rest <> bottom)

  push $ SetupStep (toTarget attrs) 1
  setAgendaDeck [Agendas.empireOfTheDead, Agendas.empireOfTheUndead]
  setActDeck
    [ Acts.throughTheCatacombs
    , Acts.thePathIsBarred
    , Acts.theWayOut
    , Acts.leadingTheWay
    ]
  whenReturnTo $ addAdditionalReferences ["52048b"]

instance RunMessage ThePallidMask where
  runMessage msg s@(ThePallidMask attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      didNotEscapeGazeOfThePhantom <- getHasRecord YouDidNotEscapeTheGazeOfThePhantom
      unableToFindNigel <- getHasRecord YouWereUnableToFindNigel
      let awokeInsideTheCatacombs = didNotEscapeGazeOfThePhantom || unableToFindNigel

      record $ if awokeInsideTheCatacombs then YouAwokeInsideTheCatacombs else YouEnteredTheCatacombsOnYourOwn

      flavor do
        h "title"
        p.validate awokeInsideTheCatacombs "readIntro1"
        p.validate (not awokeInsideTheCatacombs) "readIntro2"

      didInterview <- interviewed Assets.ishimaruHaruko
      flavor do
        h "title"
        p $ if awokeInsideTheCatacombs then "intro1" else "intro2"
        unscoped (campaignI18n (nameVar Assets.ishimaruHaruko $ p "checkIfInterviewed"))
        p.right.validate didInterview "proceedToHarukosInformation"
        p.right.validate (not didInterview) "otherwise"

      whenInterviewed Assets.ishimaruHaruko do
        flavor $ p "harukosInformation"
        remember YouOpenedASecretPassageway
      pure s
    StandaloneSetup -> do
      lead <- getLead
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      addCampaignCardToDeck lead ShuffleIn Enemies.theManInThePallidMask
      pure . ThePallidMask $ attrs & standaloneCampaignLogL .~ standaloneCampaignLog
    Setup -> runScenarioSetup ThePallidMask attrs $ setupThePallidMask attrs
    SetupStep (isTarget attrs -> True) 1 -> do
      lead <- getLead
      catacombs <- select UnrevealedLocation
      youOpenedASecretPassageway <- remembered YouOpenedASecretPassageway
      when youOpenedASecretPassageway do
        chooseTargetM lead catacombs $ push . RevealLocation (Just lead)
      pure s
    ResolveChaosToken _ Cultist iid -> do
      getSkillTestAction >>= \case
        Just Action.Fight ->
          withSkillTest \sid ->
            skillTestModifier sid Cultist iid $ if isEasyStandard attrs then DamageDealt (-1) else NoDamageDealt
        _ -> pure ()
      pure s
    ResolveChaosToken _ Tablet iid -> do
      if isEasyStandard attrs
        then do
          enemies <- select $ ReadyEnemy <> mapOneOf withTrait [Ghoul, Geist] <> enemyAtLocationWith iid
          chooseTargetM iid enemies \enemy -> initiateEnemyAttack enemy attrs iid
        else do
          enemies <- select $ mapOneOf withTrait [Ghoul, Geist] <> enemyAtLocationWith iid
          chooseTargetM iid enemies \enemy -> do
            ready enemy
            initiateEnemyAttack enemy attrs iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        ElderThing -> findAndDrawEncounterCard iid $ #enemy <> mapOneOf CardWithTrait [Ghoul, Geist]
        _ -> pure ()
      pure s
    ScenarioResolution res -> scope "resolutions" do
      investigators <- allInvestigators
      lead <- getLead
      record YouKnowTheSiteOfTheGate
      removeAllChaosTokens Cultist
      removeAllChaosTokens Tablet
      removeAllChaosTokens ElderThing
      selectForMaybeM (VictoryDisplayCardMatch $ basic $ cardIs Enemies.ishimaruHaruko) \haruko ->
        recordSetInsert VIPsSlain [toCardCode haruko]
      case res of
        NoResolution -> do
          twice $ addChaosToken ElderThing
          resolutionWithXp "noResolution" $ allGainXp' attrs
          if length investigators == 1
            then do
              gainXp lead attrs "resolutions.xp.bonus" 2
              recordSetInsert ReadActII [unInvestigatorId lead]
              searchCollectionForRandom lead attrs
                $ BasicWeaknessCard
                <> mapOneOf CardWithTrait [Madness, Pact]
            else chooseSome1M lead "Done having investigators read Act II" do
              questionLabeled "Choose who will read Act II of The King in Yellow"
              targets investigators \iid -> do
                gainXp lead attrs "resolutions.xp.bonus" 2
                recordSetInsert ReadActII [unInvestigatorId iid]
                searchCollectionForRandom iid attrs
                  $ BasicWeaknessCard
                  <> mapOneOf CardWithTrait [Madness, Pact]
        Resolution 1 -> do
          twice $ addChaosToken Cultist
          resolutionWithXp "resolution1" $ allGainXp' attrs
        Resolution 2 -> do
          twice $ addChaosToken Tablet
          chasingTheStrangerTallies <- getRecordCount ChasingTheStranger
          recordCount ChasingTheStranger (chasingTheStrangerTallies + 2)
          resolutionWithXp "resolution2" $ allGainXp' attrs
        _ -> error "Invalid resolution"

      endOfScenario
      pure s
    RequestedPlayerCard iid source mcard _ | isSource attrs source -> do
      for_ mcard $ push . AddCardToDeckForCampaign iid
      pure s
    _ -> ThePallidMask <$> liftRunMessage msg attrs
