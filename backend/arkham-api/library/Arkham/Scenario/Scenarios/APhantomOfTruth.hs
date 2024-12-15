module Arkham.Scenario.Scenarios.APhantomOfTruth (APhantomOfTruth (..), aPhantomOfTruth) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers hiding (recordSetInsert, setupModifier, skillTestModifier)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.APhantomOfTruth.Helpers
import Arkham.Scenarios.APhantomOfTruth.Story
import Arkham.Trait (Trait (Byakhee))
import Arkham.Treachery.Cards qualified as Treacheries

newtype APhantomOfTruth = APhantomOfTruth ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aPhantomOfTruth :: Difficulty -> APhantomOfTruth
aPhantomOfTruth difficulty =
  scenario
    APhantomOfTruth
    "03200"
    "A Phantom of Truth"
    difficulty
    [ "grandGuignol .                   canalSaintMartin ."
    , "grandGuignol montmartre           canalSaintMartin pèreLachaiseCemetery"
    , "opéraGarnier montmartre           leMarais         pèreLachaiseCemetery"
    , "opéraGarnier .                   leMarais         ."
    , "gareDOrsay   gardensOfLuxembourg notreDame        ."
    , "gareDOrsay   gardensOfLuxembourg notreDame        ."
    , ".            montparnasse        .                ."
    , ".            montparnasse        .                ."
    ]

instance HasChaosTokenValue APhantomOfTruth where
  getChaosTokenValue iid chaosTokenFace (APhantomOfTruth attrs) = case chaosTokenFace of
    Skull -> do
      doom <- getDoomCount
      pure $ toChaosTokenValue attrs Skull (min 5 doom) doom
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
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

cultistEffect :: ReverseQueue m => m ()
cultistEffect = do
  lead <- getLead
  byakhee <- select $ EnemyWithTrait Byakhee <> UnengagedEnemy
  byakheePairs <- forToSnd byakhee (select . NearestToEnemy . EnemyWithId)
  chooseOneAtATimeM lead do
    for_ byakheePairs \(eid, hset) -> do
      when (notNull hset) $ targeting eid (moveTowardMessages lead eid hset)
 where
  moveTowardMessages lead eid hset = case hset of
    [] -> nothing
    [x] -> moveToward eid x
    xs -> chooseOneM lead do
      for_ xs \x -> targeting x $ moveToward eid x
  moveToward eid x = push $ MoveToward (toTarget eid) (locationWithInvestigator x)

instance RunMessage APhantomOfTruth where
  runMessage msg s@(APhantomOfTruth attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      lead <- getLead
      theManInThePallidMask <- genCard Enemies.theManInThePallidMask
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      chooseOneM lead do
        labeled "Conviction" markConviction
        labeled "Doubt" markDoubt
      shuffleCardsIntoDeck lead [theManInThePallidMask]
      pure s
    PreScenarioSetup -> do
      theKingClaimedItsVictims <- getHasRecord TheKingClaimedItsVictims
      youIntrudedOnASecretMeeting <- getHasRecord YouIntrudedOnASecretMeeting
      youSlayedTheMonstersAtTheDinnerParty <- getHasRecord YouSlayedTheMonstersAtTheDinnerParty
      thePoliceAreSuspiciousOfYou <- getHasRecord ThePoliceAreSuspiciousOfYou
      chasingTheStranger <- getRecordCount ChasingTheStranger

      let
        showDream4 =
          not theKingClaimedItsVictims
            && not youIntrudedOnASecretMeeting
            && youSlayedTheMonstersAtTheDinnerParty
        showDream7 =
          not theKingClaimedItsVictims && thePoliceAreSuspiciousOfYou

      let
        dreamPath =
          catMaybes
            [ Just dream1
            , Just dream2
            , dream3 <$ guard (not theKingClaimedItsVictims && youIntrudedOnASecretMeeting)
            , dream4 <$ guard showDream4
            , dream6 <$ guard (not theKingClaimedItsVictims)
            , dream7 <$ guard showDream7
            , Just dream8
            , dream9 <$ guard (chasingTheStranger <= 3)
            , dream10 <$ guard (chasingTheStranger > 3)
            ]

      story $ if theKingClaimedItsVictims then intro1 else intro2
      traverse_ story dreamPath

      investigators <- allInvestigators
      lead <- getLead

      unlessStandalone do
        lostSouls <- replicateM 4 (genCard Treacheries.lostSoul)
        for_ (zip investigators lostSouls) \(iid, lostSoul) ->
          shuffleCardsIntoDeck iid [lostSoul]

      when showDream4 do
        paranoia <- genCard Treacheries.paranoia
        chooseTargetM lead investigators (`shuffleCardsIntoDeck` [paranoia])

      when showDream7 do
        eachInvestigator (`sufferMentalTrauma` 1)

      when (chasingTheStranger > 3) do
        chooseOneM lead do
          labeled "“How could any of this be beautiful to you?”" $ doStep 11 PreScenarioSetup
          labeled "“What exactly am I looking at?”" $ doStep 12 PreScenarioSetup
      when (chasingTheStranger <= 3) $ doStep 13 PreScenarioSetup
      pure s
    DoStep n PreScenarioSetup -> do
      when (n == 11) $ story dream11
      when (n == 12) do
        story dream12
        markDoubt
      story dream13
      story awakening

      whenInterviewed Assets.jordanPerry do
        story jordansInformation
        eachInvestigator \iid -> setupModifier attrs iid (StartingResources 3)
      pure s
    Setup -> runScenarioSetup APhantomOfTruth attrs do
      gather Set.APhantomOfTruth
      gather Set.EvilPortents
      gather Set.Byakhee
      gather Set.TheStranger
      gather Set.AgentsOfHastur

      conviction <- getRecordCount Conviction
      doubt <- getRecordCount Doubt

      if conviction > doubt
        then do
          gatherJust Set.TheMidnightMasks [Treacheries.huntingShadow]
          removeEvery [Treacheries.blackStarsRise]
          setAside [Enemies.theOrganistHopelessIDefiedHim]
        else do
          gatherJust Set.TheMidnightMasks [Treacheries.falseLead]
          removeEvery [Treacheries.twinSuns]
          setAside [Enemies.theOrganistDrapedInMystery]

      montparnasse <- place Locations.montparnasse
      gareDOrsay <- place Locations.gareDOrsay

      jordanInterviewed <- interviewed Assets.jordanPerry
      startAt $ if jordanInterviewed then montparnasse else gareDOrsay

      placeOneOf_ (Locations.montmartre209, Locations.montmartre210)
      placeOneOf_ (Locations.operaGarnier212, Locations.operaGarnier213)
      placeOneOf_ (Locations.leMarais217, Locations.leMarais218)

      placeAll
        [ Locations.grandGuignol
        , Locations.canalSaintMartin
        , Locations.pereLachaiseCemetery
        , Locations.notreDame
        , Locations.gardensOfLuxembourg
        ]

      setActDeck
        $ if conviction > doubt
          then [Acts.theParisianConspiracyV2, Acts.stalkedByShadows]
          else [Acts.theParisianConspiracyV1, Acts.pursuingShadows]

      setAgendaDeck [Agendas.theFirstNight, Agendas.theSecondNight, Agendas.theThirdNight]
    ResolveChaosToken _ Cultist _ | isHardExpert attrs -> do
      cultistEffect
      pure s
    ResolveChaosToken _ Tablet _ -> do
      withSkillTest \sid -> do
        skillTestModifier sid Tablet sid CancelSkills
        push CancelSkillEffects
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist | isEasyStandard attrs -> cultistEffect
        ElderThing -> push $ LoseResources iid (ChaosTokenEffectSource ElderThing) n
        _ -> pure ()
      pure s
    ScenarioResolution res -> scope "resolutions" do
      let
        replaceSymbolTokens token = do
          removeAllChaosTokens Cultist
          removeAllChaosTokens Tablet
          removeAllChaosTokens ElderThing
          addChaosToken token
          addChaosToken token
      case res of
        NoResolution -> do
          story noResolution
          record YouDidNotEscapeTheGazeOfThePhantom
        Resolution 1 -> do
          story resolution1
          record YouFoundNigelsHome
        Resolution 2 -> do
          story resolution2
          record YouFoundNigelEngram
        Resolution 3 -> do
          story resolution3
          record YouWereUnableToFindNigel
        _ -> error "Invalid resolution"

      when (res == Resolution 2) $ eachInvestigator (`sufferMentalTrauma` 1)

      replaceSymbolTokens $ case res of
        NoResolution -> ElderThing
        Resolution 1 -> Cultist
        Resolution 2 -> Tablet
        Resolution 3 -> ElderThing
        _ -> error "Invalid resolution"

      selectForMaybeM (VictoryDisplayCardMatch $ basic $ cardIs Enemies.jordanPerry) \jordan ->
        recordSetInsert VIPsSlain [toCardCode jordan]
      allGainXpWithBonus attrs $ if res == Resolution 2 then toBonus "insight" 2 else NoBonus
      endOfScenario
      pure s
    _ -> APhantomOfTruth <$> liftRunMessage msg attrs
