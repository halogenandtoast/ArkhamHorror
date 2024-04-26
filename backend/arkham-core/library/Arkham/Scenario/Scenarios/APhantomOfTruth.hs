module Arkham.Scenario.Scenarios.APhantomOfTruth (APhantomOfTruth (..), aPhantomOfTruth) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Deck
import Arkham.Difficulty
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Helpers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
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

gatherTheMidnightMasks :: CardGen m => Int -> Int -> m [EncounterCard]
gatherTheMidnightMasks conviction doubt = do
  traverse genEncounterCard
    $ if conviction > doubt
      then replicate 3 Treacheries.huntingShadow
      else replicate 2 Treacheries.falseLead

cultistEffect :: (HasGame m, HasQueue Message m) => m ()
cultistEffect = do
  lead <- getLeadPlayer
  byakhee <- select $ EnemyWithTrait Byakhee <> UnengagedEnemy
  byakheePairs <- forToSnd byakhee (select . NearestToEnemy . EnemyWithId)
  push
    $ chooseOneAtATime
      lead
      [ targetLabel eid (moveTowardMessages lead eid hset)
      | (eid, hset) <- byakheePairs
      ]
 where
  moveTowardMessages lead eid hset = case hset of
    [] -> []
    [x] -> [moveToward eid x]
    xs -> [chooseOne lead [targetLabel x [moveToward eid x] | x <- xs]]
  moveToward eid x = MoveToward (toTarget eid) (locationWithInvestigator x)

instance RunMessage APhantomOfTruth where
  runMessage msg s@(APhantomOfTruth attrs) = case msg of
    SetChaosTokensForScenario -> do
      whenM getIsStandalone $ do
        randomToken <- sample (Cultist :| [Tablet, ElderThing])
        push (SetChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken])
      pure s
    StandaloneSetup -> do
      lead <- getLeadPlayer
      leadId <- getLeadInvestigatorId
      theManInThePallidMask <- genCard Enemies.theManInThePallidMask
      pushAll
        [ chooseOne
            lead
            [ Label "Conviction" [RecordCount Conviction 1]
            , Label "Doubt" [RecordCount Doubt 1]
            ]
        , ShuffleCardsIntoDeck (InvestigatorDeck leadId) [theManInThePallidMask]
        ]
      pure s
    PreScenarioSetup -> do
      investigatorIds <- allInvestigatorIds
      lead <- getLeadPlayer
      players <- allPlayers

      theKingClaimedItsVictims <- getHasRecord TheKingClaimedItsVictims
      youIntrudedOnASecretMeeting <- getHasRecord YouIntrudedOnASecretMeeting
      youSlayedTheMonstersAtTheDinnerParty <-
        getHasRecord
          YouSlayedTheMonstersAtTheDinnerParty
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
        intro = if theKingClaimedItsVictims then intro1 else intro2
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

      paranoia <- genCard Treacheries.paranoia
      lostSouls <- replicateM 4 (genCard Treacheries.lostSoul)
      standalone <- getIsStandalone

      pushAll
        $ story players intro
        : map (story players) dreamPath
          <> [ ShuffleCardsIntoDeck (InvestigatorDeck iid) [lostSoul]
             | not standalone
             , (iid, lostSoul) <- zip investigatorIds lostSouls
             ]
          <> [ chooseOne
              lead
              [ targetLabel
                iid
                [ShuffleCardsIntoDeck (InvestigatorDeck iid) [paranoia]]
              | iid <- investigatorIds
              ]
             | showDream4
             ]
          <> [SufferTrauma iid 0 1 | showDream7, iid <- investigatorIds]
          <> [ chooseOne
              lead
              [ Label
                  "“How could any of this be beautiful to you?”"
                  [DoStep 11 PreScenarioSetup]
              , Label
                  "“What exactly am I looking at?”"
                  [DoStep 12 PreScenarioSetup]
              ]
             | chasingTheStranger > 3
             ]
          <> [DoStep 13 PreScenarioSetup | chasingTheStranger <= 3]
      pure s
    DoStep n PreScenarioSetup -> do
      doubt <- getRecordCount Doubt

      jordanInterviewed <- interviewed Assets.jordanPerry
      investigatorIds <- allInvestigatorIds
      players <- allPlayers

      pushAll
        $ [story players dream11 | n == 11]
        <> [story players dream12 | n == 12]
        <> [RecordCount Doubt (doubt + 1) | n == 12]
        <> [story players dream13, story players awakening]
        <> [story players jordansInformation | jordanInterviewed]
        <> [setupModifier attrs iid (StartingResources 3) | jordanInterviewed, iid <- investigatorIds]
      pure s
    Setup -> do
      conviction <- getRecordCount Conviction
      doubt <- getRecordCount Doubt

      let
        act1 =
          if conviction > doubt
            then Acts.theParisianConspiracyV2
            else Acts.theParisianConspiracyV1
        act2 =
          if conviction > doubt
            then Acts.stalkedByShadows
            else Acts.pursuingShadows
        excludes =
          if conviction > doubt
            then [Treacheries.blackStarsRise]
            else [Treacheries.twinSuns]
        theOrganist =
          if conviction > doubt
            then Enemies.theOrganistHopelessIDefiedHim
            else Enemies.theOrganistDrapedInMystery

      gatheredCards <-
        buildEncounterDeckExcluding
          excludes
          [ EncounterSet.APhantomOfTruth
          , EncounterSet.EvilPortents
          , EncounterSet.Byakhee
          , EncounterSet.TheStranger
          , EncounterSet.AgentsOfHastur
          ]
      midnightMasks <- gatherTheMidnightMasks conviction doubt
      encounterDeck <- Deck <$> shuffleM (unDeck gatheredCards <> midnightMasks)

      setAsideCards <- genCards [theOrganist]

      montmartre <- genCard =<< sample2 Locations.montmartre209 Locations.montmartre210
      operaGarnier <- genCard =<< sample2 Locations.operaGarnier212 Locations.operaGarnier213
      leMarais <- genCard =<< sample2 Locations.leMarais217 Locations.leMarais218

      montparnasse <- genCard Locations.montparnasse
      grandGuignol <- genCard Locations.grandGuignol
      gareDOrsay <- genCard Locations.gareDOrsay
      pereLachaiseCemetery <- genCard Locations.pereLachaiseCemetery
      canalSaintMartin <- genCard Locations.canalSaintMartin
      notreDame <- genCard Locations.notreDame
      gardensOfLuxembourg <- genCard Locations.gardensOfLuxembourg

      jordanInterviewed <- interviewed Assets.jordanPerry

      (montparnasseId, placeMontparnasse) <- placeLocation montparnasse
      (gareDOrsayId, placeGareDOrsay) <- placeLocation gareDOrsay

      let
        startingLocation =
          if jordanInterviewed then montparnasseId else gareDOrsayId

      otherPlacements <-
        traverse
          placeLocation_
          [ montmartre
          , operaGarnier
          , leMarais
          , grandGuignol
          , canalSaintMartin
          , pereLachaiseCemetery
          , notreDame
          , gardensOfLuxembourg
          ]

      pushAll
        $ [SetEncounterDeck encounterDeck, SetAgendaDeck, SetActDeck]
        <> (placeMontparnasse : placeGareDOrsay : otherPlacements)
        <> [MoveAllTo (toSource attrs) startingLocation]

      acts <- genCards [act1, act2]
      agendas <-
        genCards
          [Agendas.theFirstNight, Agendas.theSecondNight, Agendas.theThirdNight]

      APhantomOfTruth
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Cultist _ -> do
      when (isHardExpert attrs) cultistEffect
      pure s
    ResolveChaosToken _ Tablet _ -> do
      pushAll
        [ CreateWindowModifierEffect
            EffectSkillTestWindow
            (EffectModifiers $ toModifiers attrs [CancelSkills])
            (ChaosTokenEffectSource Tablet)
            SkillTestTarget
        , CancelSkillEffects
        ]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> case chaosTokenFace token of
      Cultist | isEasyStandard attrs -> s <$ cultistEffect
      ElderThing ->
        s <$ push (LoseResources iid (ChaosTokenEffectSource ElderThing) n)
      _ -> pure s
    ScenarioResolution res -> do
      investigatorIds <- allInvestigatorIds
      players <- allPlayers
      jordanSlain <-
        selectOne
          (VictoryDisplayCardMatch $ cardIs Enemies.jordanPerry)
      gainXp <-
        toGainXp attrs
          $ getXpWithBonus (if res == Resolution 2 then 2 else 0)
      let
        updateSlain =
          [ recordSetInsert VIPsSlain [toCardCode jordan]
          | jordan <- maybeToList jordanSlain
          ]
        sufferTrauma =
          if res == Resolution 2
            then [SufferTrauma iid 0 1 | iid <- investigatorIds]
            else []
        (storyText, record, token) = case res of
          NoResolution ->
            (noResolution, YouDidNotEscapeTheGazeOfThePhantom, ElderThing)
          Resolution 1 -> (resolution1, YouFoundNigelsHome, Cultist)
          Resolution 2 -> (resolution2, YouFoundNigelEngram, Tablet)
          Resolution 3 -> (resolution3, YouWereUnableToFindNigel, ElderThing)
          _ -> error "Invalid resolution"
      pushAll
        $ [story players storyText, Record record]
        <> sufferTrauma
        <> [ RemoveAllChaosTokens Cultist
           , RemoveAllChaosTokens Tablet
           , RemoveAllChaosTokens ElderThing
           , AddChaosToken token
           , AddChaosToken token
           ]
        <> updateSlain
        <> gainXp
        <> [EndOfGame Nothing]
      pure s
    _ -> APhantomOfTruth <$> runMessage msg attrs
