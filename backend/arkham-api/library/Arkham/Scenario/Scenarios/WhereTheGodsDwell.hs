module Arkham.Scenario.Scenarios.WhereTheGodsDwell (whereTheGodsDwell) where

import Arkham.Act.CardDefs.TheDreamEaters.WhereTheGodsDwell qualified as Acts
import Arkham.Agenda.CardDefs.TheDreamEaters.WhereTheGodsDwell qualified as Agendas
import Arkham.Attack
import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Helpers.FlavorText (li, setup, ul)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.WhereTheGodsDwell.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype WhereTheGodsDwell = WhereTheGodsDwell ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereTheGodsDwell :: Difficulty -> WhereTheGodsDwell
whereTheGodsDwell difficulty =
  scenario
    WhereTheGodsDwell
    "06286"
    "Where the Gods Dwell"
    difficulty
    [ ".               plateauOfLeng  ."
    , "monasteryOfLeng coldWastes     ."
    , ".               onyxGates      theOnyxCastle"
    ]

instance HasChaosTokenValue WhereTheGodsDwell where
  getChaosTokenValue iid tokenFace (WhereTheGodsDwell attrs) = case tokenFace of
    Skull -> do
      n <- getCurrentActStep
      m <- getCurrentAgendaStep
      pure $ toChaosTokenValue attrs Skull n (n + m)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 4 6
    ElderThing -> pure $ ChaosTokenValue ElderThing $ byDifficulty attrs ZeroModifier (NegativeModifier 1)
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
  , Skull
  , Cultist
  , Tablet
  , Tablet
  , AutoFail
  , ElderSign
  ]

instance RunMessage WhereTheGodsDwell where
  runMessage msg s@(WhereTheGodsDwell attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      carried <- getHasRecord TheInvestigatorsWereCarriedToTheColdWastes
      story $ i18nWithTitle $ if carried then "intro1" else "intro2"
      pure s
    StandaloneSetup -> do
      record RandolphSurvivedTheVoyage
      record TheInvestigatorsTraveledToTheColdWastes
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup WhereTheGodsDwell attrs do
      setup do
        ul do
          li "gatherSets"
          li.nested "putLocations" do
            li "beginAtPlateauOfLeng"
            li "setForsakenTowersAside"
          li "setCardsAside"
          li "buildEncounterDeck"

      gather Set.WhereTheGodsDwell
      gather Set.AgentsOfNyarlathotep
      gather Set.DreamersCurse
      gather Set.WhispersOfHypnos
      gather Set.DarkCult

      setAgendaDeck [Agendas.theEyeOfChaos, Agendas.theShapeOfChaos, Agendas.chaosIncarnate]
      setActDeck
        [ Acts.journeyThroughTheColdWastes
        , Acts.theThingInTheRobes
        , Acts.beyondDreams
        , Acts.truthAndLies
        , Acts.theDreamEaters -- that's the name of the thing!
        ]

      startAt =<< place Locations.plateauOfLengWhereTheGodsDwell
      placeAll
        [ Locations.coldWastes
        , Locations.monasteryOfLeng
        , Locations.onyxGates
        , Locations.theOnyxCastle
        ]

      setAside
        [ Locations.forsakenTowerOfIllusionAndMyth
        , Locations.forsakenTowerOfLifeAndDeath
        , Locations.forsakenTowerOfInfiniteTruth
        , Locations.forsakenTowerOfEternalFlame
        , Locations.forsakenTowerOfTheQueenOfNight
        , Locations.forsakenTowerOfPrimevalLight
        , Enemies.highPriestNotToBeDescribed
        , Enemies.nyarlathotepTheCrawlingChaos
        , Enemies.nyarlathotepTheFacelessWhisperer
        , Enemies.nyarlathotepMessengerOfTheOuterGods
        , Enemies.nyarlathotepGodOfAThousandForms
        , Enemies.nyarlathotepStalkerAmongTheStars
        , Treacheries.whisperingChaosNorth
        , Treacheries.whisperingChaosSouth
        , Treacheries.whisperingChaosEast
        , Treacheries.whisperingChaosWest
        , Treacheries.myriadForms
        , Treacheries.myriadForms
        , Enemies.theCrawlingMist
        ]
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist ->
          byDifficulty
            attrs
            (placeDoomOnAgenda 1)
            (placeDoomOnAgendaAndCheckAdvance 1)
        Tablet -> do
          nyarlathoteps <- select $ EnemyWithPlacement (HiddenInHand iid) <> EnemyWithTitle "Nyarlathotep"
          when (notNull nyarlathoteps)
            $ chooseOne
              iid
              [ targetLabel
                  nyarlathotep
                  [ InitiateEnemyAttack $ enemyAttack nyarlathotep TabletEffect iid
                  , ShuffleBackIntoEncounterDeck GameSource (toTarget nyarlathotep)
                  ]
              | nyarlathotep <- nyarlathoteps
              ]
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          record Nyarlathotep'sInvasionHasBegun
          whenM getIsTheDreamQuest $ push GameOver
          endOfScenario
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          record TheDreamersEscapedFromNyarlathotep'sGrasp
          eachInvestigator (`sufferMentalTrauma` 2)
          lead <- getLead
          knowOfAnotherPath <- getHasRecord TheDreamersKnowOfAnotherPath
          chooseOne
            lead
            $ [ Label "$theDreamEaters.whereTheGodsDwell.label.wakeUp" [R3]
              , Label "$theDreamEaters.whereTheGodsDwell.label.remainOnSurface" [R4]
              ]
            <> [ Label "$theDreamEaters.whereTheGodsDwell.label.ventureIntoUnderworld" [R5]
               | knowOfAnotherPath
               ]
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "resolution2" 5
          record TheDreamersBanishedNyarlathotep
          eachInvestigator (`sufferMentalTrauma` 2)
          lead <- getLead
          knowOfAnotherPath <- getHasRecord TheDreamersKnowOfAnotherPath
          chooseOne
            lead
            $ [ Label "$theDreamEaters.whereTheGodsDwell.label.wakeUp" [R3]
              , Label "$theDreamEaters.whereTheGodsDwell.label.remainOnSurface" [R4]
              ]
            <> [ Label "$theDreamEaters.whereTheGodsDwell.label.ventureIntoUnderworld" [R5]
               | knowOfAnotherPath
               ]
        Resolution 3 -> do
          resolution "resolution3"
          record TheDreamersAwoke
          whenM getIsTheDreamQuest $ push GameOver
          endOfScenario
        Resolution 4 -> do
          resolution "resolution4"
          record TheDreamersStayedInTheDreamlandsForever
          whenM getIsTheDreamQuest $ push GameOver
          endOfScenario
        Resolution 5 -> do
          resolution "resolution5"
          record TheDreamersTraveledBeneathTheMonastery
          endOfScenario
        other -> throw $ UnknownResolution other

      pure s
    _ -> WhereTheGodsDwell <$> liftRunMessage msg attrs
