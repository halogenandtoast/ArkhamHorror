module Arkham.Scenario.Scenarios.WhereTheGodsDwell (WhereTheGodsDwell (..), whereTheGodsDwell) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Attack
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
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
    [ ".               plateauOfLeng  .              ."
    , "monasteryOfLeng coldWastes     forsakenTower1 forsakenTower2"
    , ".               onyxGates      theOnyxCastle  forsakenTower3"
    , ".               forsakenTower4 forsakenTower5 forsakenTower6"
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

instance RunMessage WhereTheGodsDwell where
  runMessage msg s@(WhereTheGodsDwell attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      carried <- getHasRecord TheInvestigatorsWereCarriedToTheColdWastes
      story
        $ i18nWithTitle
        $ if carried then "dreamEaters.whereTheGodsDwell.intro1" else "dreamEaters.whereTheGodsDwell.intro2"
      pure s
    Setup -> runScenarioSetup WhereTheGodsDwell attrs do
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
          nyarlathoteps <- select $ EnemyWithPlacement (StillInHand iid) <> EnemyWithTitle "Nyarlathotep"
          chooseOne
            iid
            [ targetLabel
              nyarlathotep
              [ InitiateEnemyAttack $ enemyAttack nyarlathotep TabletEffect iid
              , ShuffleBackIntoEncounterDeck (toTarget nyarlathotep)
              ]
            | nyarlathotep <- nyarlathoteps
            ]
        _ -> pure ()
      pure s
    _ -> WhereTheGodsDwell <$> lift (runMessage msg attrs)
