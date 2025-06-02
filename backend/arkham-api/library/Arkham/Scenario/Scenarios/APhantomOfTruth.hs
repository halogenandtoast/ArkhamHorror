module Arkham.Scenario.Scenarios.APhantomOfTruth (setupAPhantomOfTruth, aPhantomOfTruth, APhantomOfTruth (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.ThePathToCarcosa.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Doom
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.APhantomOfTruth.Helpers
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

setupAPhantomOfTruth :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupAPhantomOfTruth _attrs = do
  setup do
    ul do
      li "gatherSets"
      li.nested "doubt.instructions" do
        li "doubt.remove"
        li "doubt.act"
      li.nested "conviction.instructions" do
        li "conviction.remove"
        li "conviction.act"
      li "chooseLocations"
      li "placeLocations"
      li "setAside"
      li "lostSoul"
      unscoped $ li "shuffleRemainder"

  whenReturnTo $ gather Set.ReturnToAPhantomOfTruth
  gather Set.APhantomOfTruth
  gather Set.EvilPortents
  gather Set.Byakhee
  gather Set.TheStranger
  gather Set.AgentsOfHastur `orWhenReturnTo` gather Set.HastursEnvoys

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

  montparnasse <-
    place Locations.montparnasse
      `orWhenReturnTo` placeOneOf (Locations.montparnasse, Locations.returnToMontparnasse)
  gareDOrsay <- place Locations.gareDOrsay

  jordanInterviewed <- interviewed Assets.jordanPerry
  startAt $ if jordanInterviewed then montparnasse else gareDOrsay

  placeOneOf_ (Locations.montmartre209, Locations.montmartre210)
  placeOneOf_ (Locations.operaGarnier212, Locations.operaGarnier213)
  placeOneOf_ (Locations.leMarais217, Locations.leMarais218)

  isReturnTo <- getIsReturnTo
  if not isReturnTo
    then
      placeAll
        [ Locations.grandGuignol
        , Locations.canalSaintMartin
        , Locations.pereLachaiseCemetery
        , Locations.notreDame
        , Locations.gardensOfLuxembourg
        ]
    else do
      placeOneOf_ (Locations.grandGuignol, Locations.returnToGrandGuignol)
      placeOneOf_ (Locations.canalSaintMartin, Locations.returnToCanalSaintMartin)
      placeOneOf_ (Locations.pereLachaiseCemetery, Locations.returnToPereLachaiseCemetery)
      placeOneOf_ (Locations.notreDame, Locations.returnToNotreDame)
      placeOneOf_ (Locations.gardensOfLuxembourg, Locations.returnToGardensOfLuxembourg)
      setAside [Treacheries.figureInTheShadows, Treacheries.figureInTheShadows]
      addAdditionalReferences ["52040b"]

  setActDeck
    $ if conviction > doubt
      then [Acts.theParisianConspiracyV2, Acts.stalkedByShadows]
      else [Acts.theParisianConspiracyV1, Acts.pursuingShadows]

  setAgendaDeck [Agendas.theFirstNight, Agendas.theSecondNight, Agendas.theThirdNight]

instance RunMessage APhantomOfTruth where
  runMessage msg s@(APhantomOfTruth attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      lead <- getLead
      theManInThePallidMask <- genCard Enemies.theManInThePallidMask
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      chooseOneM lead $ popScope do
        labeled' "key.conviction" markConviction
        labeled' "key.doubt" markDoubt
      addCampaignCardToDeck lead ShuffleIn theManInThePallidMask
      pure s
    PreScenarioSetup -> scope "intro" do
      theKingClaimedItsVictims <- getHasRecord TheKingClaimedItsVictims
      youIntrudedOnASecretMeeting <- getHasRecord YouIntrudedOnASecretMeeting
      youSlayedTheMonstersAtTheDinnerParty <- getHasRecord YouSlayedTheMonstersAtTheDinnerParty
      thePoliceAreSuspiciousOfYou <- getHasRecord ThePoliceAreSuspiciousOfYou
      chasingTheStranger <- getRecordCount ChasingTheStranger
      investigators <- allInvestigators

      let
        showDream4 =
          not theKingClaimedItsVictims
            && not youIntrudedOnASecretMeeting
            && youSlayedTheMonstersAtTheDinnerParty
        showDream7 = not theKingClaimedItsVictims && thePoliceAreSuspiciousOfYou

      flavor do
        h "title"
        p.validate theKingClaimedItsVictims "readIntro1"
        p.validate (not theKingClaimedItsVictims) "readIntro2"
      flavor $ h "title" >> p (if theKingClaimedItsVictims then "intro1" else "intro2")

      flavor $ h "title" >> p "dream1"
      unlessStandalone do
        lostSouls <- replicateM 4 (genCard Treacheries.lostSoul)
        for_ (zip investigators lostSouls) \(iid, lostSoul) ->
          shuffleCardsIntoDeck iid [lostSoul]

      flavor do
        h "title"
        p "dream2.body"
        p.validate theKingClaimedItsVictims "dream2.dream8"
        p.validate (not theKingClaimedItsVictims && youIntrudedOnASecretMeeting) "dream2.dream3"
        p.validate showDream4 "dream2.dream4"
        p.validate
          ( not
              $ or [theKingClaimedItsVictims, youIntrudedOnASecretMeeting, youSlayedTheMonstersAtTheDinnerParty]
          )
          "dream2.dream6"

      when (not theKingClaimedItsVictims && youIntrudedOnASecretMeeting) do
        flavor $ h "title" >> p "dream3"

      when showDream4 do
        flavor $ h "title" >> p "dream4"
        eachInvestigator (`sufferMentalTrauma` 1)

      unless theKingClaimedItsVictims do
        flavor do
          h "title"
          p "dream6.body"
          p.validate showDream7 "dream6.dream7"
          p.validate (not showDream7) "dream6.dream8"

      when showDream7 do
        flavor $ h "title" >> p "dream7"
        paranoia <- genCard Treacheries.paranoia
        lead <- getLead
        chooseTargetM lead investigators (`shuffleCardsIntoDeck` [paranoia])

      flavor do
        h "title"
        p "dream8.body"
        p.validate (chasingTheStranger <= 3) "dream8.dream9"
        p.validate (chasingTheStranger > 3) "dream8.dream10"

      when (chasingTheStranger <= 3) do
        flavor $ h "title" >> p "dream9"
        doStep 13 PreScenarioSetup

      when (chasingTheStranger > 3) do
        storyWithChooseOneM' (h "title" >> p "dream10") do
          labeled' "dream10.dream11" $ doStep 11 PreScenarioSetup
          labeled' "dream10.dream12" $ doStep 12 PreScenarioSetup

      pure s
    DoStep n PreScenarioSetup -> scope "intro" do
      when (n == 11) do
        flavor $ h "title" >> p "dream11"
        markConviction
      when (n == 12) do
        flavor $ h "title" >> p "dream12"
        markDoubt
      flavor $ h "title" >> p "dream13"

      didInterview <- interviewed Assets.jordanPerry
      flavor do
        h "title"
        p "awakening"
        unscoped (campaignI18n (nameVar Assets.jordanPerry $ p "checkIfInterviewed"))
        p.right.validate didInterview "proceedToJordansInformation"
        p.right.validate (not didInterview) "otherwise"
      -- story intro
      whenInterviewed Assets.jordanPerry do
        flavor $ p "jordansInformation"
        eachInvestigator \iid -> setupModifier attrs iid (StartingResources 3)
      pure s
    Setup -> runScenarioSetup APhantomOfTruth attrs $ setupAPhantomOfTruth attrs
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
    ScenarioResolution r -> scope "resolutions" do
      let
        replaceSymbolTokens token = do
          removeAllChaosTokens Cultist
          removeAllChaosTokens Tablet
          removeAllChaosTokens ElderThing
          addChaosToken token
          addChaosToken token
      case r of
        NoResolution -> do
          resolutionWithXp "noResolution" $ allGainXp' attrs
          record YouDidNotEscapeTheGazeOfThePhantom
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          record YouFoundNigelsHome
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs (toBonus "bonus" 2)
          record YouFoundNigelEngram
          eachInvestigator (`sufferMentalTrauma` 1)
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXp' attrs
          record YouWereUnableToFindNigel
        _ -> error "Invalid resolution"

      replaceSymbolTokens $ case r of
        NoResolution -> ElderThing
        Resolution 1 -> Cultist
        Resolution 2 -> Tablet
        Resolution 3 -> ElderThing
        _ -> error "Invalid resolution"

      selectForMaybeM (VictoryDisplayCardMatch $ basic $ cardIs Enemies.jordanPerry) \jordan ->
        recordSetInsert VIPsSlain [toCardCode jordan]
      endOfScenario
      pure s
    UseCardAbility _ ScenarioSource 1 _ _ -> do
      shuffleSetAsideIntoEncounterDeck $ cardIs Treacheries.figureInTheShadows
      pure s
    _ -> APhantomOfTruth <$> liftRunMessage msg attrs
