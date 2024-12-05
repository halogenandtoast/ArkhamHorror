module Arkham.Scenario.Scenarios.ThePallidMask (ThePallidMask (..), thePallidMask) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Distance
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers hiding (recordSetInsert, skillTestModifier)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Label (unLabel)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.Scenarios.ThePallidMask.Story
import Arkham.SkillTest
import Arkham.Token
import Arkham.Trait (Trait (Geist, Ghoul, Madness, Pact))

newtype ThePallidMask = ThePallidMask ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Locations are placed directional, printed on the cards the following
-- directions are possible:
--
-- Left: 2
-- Right: 11
-- Above: 5
-- Below: 6
--
-- This means that our starting position is 2,6 and we have a grid that is
-- 13 x 11, in order to convert we use labels to figure out the new position

thePallidMask :: Difficulty -> ThePallidMask
thePallidMask difficulty =
  scenario
    ThePallidMask
    "03240"
    "The Pallid Mask"
    difficulty
    [ "pos0011 pos0111 pos0211 pos0311 pos0411 pos0511 pos0611 pos0711 pos0811 pos0911 pos1011 pos1111 pos1211 pos1311"
    , "pos0010 pos0110 pos0210 pos0310 pos0410 pos0510 pos0610 pos0710 pos0810 pos0910 pos1010 pos1110 pos1210 pos1310"
    , "pos0009 pos0109 pos0209 pos0309 pos0409 pos0509 pos0609 pos0709 pos0809 pos0909 pos1009 pos1109 pos1209 pos1309"
    , "pos0008 pos0108 pos0208 pos0308 pos0408 pos0508 pos0608 pos0708 pos0808 pos0908 pos1008 pos1108 pos1208 pos1308"
    , "pos0007 pos0107 pos0207 pos0307 pos0407 pos0507 pos0607 pos0707 pos0807 pos0907 pos1007 pos1107 pos1207 pos1307"
    , "pos0006 pos0106 pos0206 pos0306 pos0406 pos0506 pos0606 pos0706 pos0806 pos0906 pos1006 pos1106 pos1206 pos1306"
    , "pos0005 pos0105 pos0205 pos0305 pos0405 pos0505 pos0605 pos0705 pos0805 pos0905 pos1005 pos1105 pos1205 pos1305"
    , "pos0004 pos0104 pos0204 pos0304 pos0404 pos0504 pos0604 pos0704 pos0804 pos0904 pos1004 pos1104 pos1204 pos1304"
    , "pos0003 pos0103 pos0203 pos0303 pos0403 pos0503 pos0603 pos0703 pos0803 pos0903 pos1003 pos1103 pos1203 pos1303"
    , "pos0002 pos0102 pos0202 pos0302 pos0402 pos0502 pos0602 pos0702 pos0802 pos0902 pos1002 pos1102 pos1202 pos1302"
    , "pos0001 pos0101 pos0201 pos0301 pos0401 pos0501 pos0601 pos0701 pos0801 pos0901 pos1001 pos1101 pos1201 pos1301"
    , "pos0000 pos0100 pos0200 pos0300 pos0400 pos0500 pos0600 pos0700 pos0800 pos0900 pos1000 pos1100 pos1200 pos1300"
    ]

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
          [YouFoundNigelsHome, YouEnteredTheCatacombsOnYourOwn]
    }

instance HasChaosTokenValue ThePallidMask where
  getChaosTokenValue iid chaosTokenFace (ThePallidMask attrs) = case chaosTokenFace of
    Skull -> do
      -- -X where X is the number of locations away from the starting location
      startingLocation <-
        selectJust
          $ LocationWithLabel
          $ positionToLabel
            startPosition
      yourLocation <-
        fromJustNote "no location" <$> field InvestigatorLocation iid
      distance <-
        unDistance
          . fromMaybe (Distance 0)
          <$> getDistance startingLocation yourLocation
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

instance RunMessage ThePallidMask where
  runMessage msg s@(ThePallidMask attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      didNotEscapeGazeOfThePhantom <- getHasRecord YouDidNotEscapeTheGazeOfThePhantom
      unableToFindNigel <- getHasRecord YouWereUnableToFindNigel
      let awokeInsideTheCatacombs = didNotEscapeGazeOfThePhantom || unableToFindNigel
      story $ if awokeInsideTheCatacombs then intro1 else intro2
      whenInterviewed Assets.ishimaruHaruko do
        story harukosInformation
        remember YouOpenedASecretPassageway
      pure s
    StandaloneSetup -> do
      lead <- getLead
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      addCampaignCardToDeck lead Enemies.theManInThePallidMask
      pure . ThePallidMask $ attrs & standaloneCampaignLogL .~ standaloneCampaignLog
    Setup -> runScenarioSetup ThePallidMask attrs do
      gather Set.ThePallidMask
      gather Set.Ghouls
      gather Set.Hauntings
      gather Set.ChillingCold

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

      didNotEscapeGazeOfThePhantom <- getHasRecord YouDidNotEscapeTheGazeOfThePhantom
      unableToFindNigel <- getHasRecord YouWereUnableToFindNigel
      let awokeInsideTheCatacombs = didNotEscapeGazeOfThePhantom || unableToFindNigel
      let startLabel = unLabel $ positionToLabel startPosition
      (start, remainingCatacombs) <-
        if awokeInsideTheCatacombs
          then do
            shuffled <- shuffleM (Locations.theGateToHell : otherCatacombs)
            case shuffled of
              (x : xs) -> (,xs) <$> placeLabeled startLabel x
              _ -> error "invalid setup"
          else (,) <$> placeLabeled startLabel Locations.theGateToHell <*> shuffleM otherCatacombs

      startAt start
      placeTokens attrs start Resource 1

      theManInThePallidMask <- getCampaignStoryCard Enemies.theManInThePallidMask
      push $ RemoveFromBearersDeckOrDiscard theManInThePallidMask
      setAside [Enemies.theManInThePallidMask]

      let (bottom3, rest) = splitAt 3 remainingCatacombs
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
    ScenarioResolution res -> do
      investigators <- allInvestigators
      lead <- getLead
      let
        (token, story') = case res of
          NoResolution -> (ElderThing, noResolution)
          Resolution 1 -> (Cultist, resolution1)
          Resolution 2 -> (Tablet, resolution2)
          _ -> error "Invalid resolution"
      story story'
      record YouKnowTheSiteOfTheGate

      if length investigators == 1
        then do
          recordSetInsert ReadActII [unInvestigatorId lead]
          searchCollectionForRandom lead attrs
            $ BasicWeaknessCard
            <> mapOneOf CardWithTrait [Madness, Pact]
        else chooseSome1M lead "Done having investigators read Act II" do
          questionLabeled "Choose who will read Act II of The King in Yellow"
          targets investigators \iid -> do
            recordSetInsert ReadActII [unInvestigatorId iid]
            searchCollectionForRandom iid attrs
              $ BasicWeaknessCard
              <> mapOneOf CardWithTrait [Madness, Pact]

      removeAllChaosTokens Cultist
      removeAllChaosTokens Tablet
      removeAllChaosTokens ElderThing
      addChaosToken token
      addChaosToken token

      when (res == Resolution 2) $ do
        chasingTheStrangerTallies <- getRecordCount ChasingTheStranger
        recordCount ChasingTheStranger (chasingTheStrangerTallies + 2)

      selectForMaybeM (VictoryDisplayCardMatch $ basic $ cardIs Enemies.ishimaruHaruko) \haruko ->
        recordSetInsert VIPsSlain [toCardCode haruko]
      doStep 1 msg
      pure s
    DoStep 1 (ScenarioResolution _) -> do
      allGainXp attrs
      endOfScenario
      pure s
    RequestedPlayerCard iid source mcard _ | isSource attrs source -> do
      for_ mcard $ push . AddCardToDeckForCampaign iid
      pure s
    _ -> ThePallidMask <$> liftRunMessage msg attrs
