module Arkham.Scenario.Scenarios.TheSecretName (setupTheSecretName, theSecretName, TheSecretName (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (EnemyDefeated, RevealLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheSecretName.Helpers
import Arkham.Trait (Trait (Extradimensional))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Zone

newtype TheSecretName = TheSecretName ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecretName :: Difficulty -> TheSecretName
theSecretName difficulty =
  scenario
    TheSecretName
    "05120"
    "The Secret Name"
    difficulty
    [ ".              .                 .             unknownPlaces4           unknownPlaces1   unknownPlaces2         unknownPlaces3 ."
    , ".              walterGilmansRoom .             cityOfElderThings        physicsClassroom siteOfTheSacrifice     .              strangeGeometry1"
    , "decrepitDoor1  moldyHalls        decrepitDoor2 moldyHallsEarlierTonight keziahsRoom      witchHouseRuins        .              ."
    , ".              decrepitDoor3     .             salemGaol1692            twilightAbyss    courtOfTheGreatOldOnes .              strangeGeometry2"
    , ".              .                 .             .                        unknownPlaces5   unknownPlaces6         unknownPlaces7 ."
    ]

instance HasChaosTokenValue TheSecretName where
  getChaosTokenValue iid chaosTokenFace (TheSecretName attrs) = case chaosTokenFace of
    Skull -> do
      atExtradimensionalLocation <-
        selectAny $ locationWithInvestigator iid <> LocationWithTrait Extradimensional
      pure
        $ if atExtradimensionalLocation
          then toChaosTokenValue attrs Skull 3 4
          else toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

{- FOURMOLU_DISABLE -}
standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour
  , Skull , Skull , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

setupTheSecretName :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheSecretName _attrs = do
  setup $ ul do
    li "gatherSets"
    li "placeLocations"
    li.nested "unknownPlaces" do
      li "bottom"
      li "top"
    li "setAside"
    unscoped $ li "shuffleRemainder"

  whenReturnTo $ gather Set.ReturnToTheSecretName
  gather Set.TheSecretName
  gather Set.CityOfSins `orWhenReturnTo` gather Set.CityOfTheDamned
  gather Set.InexorableFate `orWhenReturnTo` gather Set.UnspeakableFate
  gather Set.RealmOfDeath `orWhenReturnTo` gather Set.UnstableRealm
  gather Set.Witchcraft `orWhenReturnTo` gather Set.Hexcraft
  gather Set.Rats

  setAgendaDeck
    [ Agendas.theHermitIX
    , Agendas.theFamiliar
    , Agendas.theWitchLight
    , Agendas.markedForSacrifice
    ]
  setActDeck
    [ Acts.investigatingTheWitchHouse
    , Acts.beyondTheWitchHouse
    , Acts.stoppingTheRitual
    ]

  startAt =<< place Locations.moldyHalls
  place_ Locations.walterGilmansRoom

  placeGroup
    "decrepitDoor"
    [ Locations.landlordsQuarters
    , Locations.joeMazurewiczsRoom
    , Locations.frankElwoodsRoom
    ]

  placeEnemy Enemies.nahab (OutOfPlay SetAsideZone)

  setAside
    [ Locations.siteOfTheSacrifice
    , Locations.keziahsRoom
    , Assets.theBlackBook
    , Locations.strangeGeometry
    , Locations.strangeGeometry
    , Treacheries.ghostlyPresence
    , Treacheries.ghostlyPresence
    ]

  isReturnTo <- getIsReturnTo
  -- Unknown Places Deck
  unknownPlaces <-
    fmap (if isReturnTo then drop 4 else id)
      . shuffleM
      =<< genCards
        ( [ Locations.moldyHallsEarlierTonight
          , Locations.twilightAbyss
          , Locations.cityOfElderThings
          , Locations.salemGaol1692
          , Locations.physicsClassroom
          , Locations.courtOfTheGreatOldOnesANotTooDistantFuture
          ]
            <> ( guard isReturnTo
                   *> [ Locations.templeOfRlyeh
                      , Locations.thePriceManor
                      , Locations.the9thWard
                      , Locations.libraryOfEbla
                      ]
               )
        )

  let (bottom, top) = splitAt 3 unknownPlaces
  witchHouseRuins <- genCard Locations.witchHouseRuins
  bottom' <- shuffleM $ witchHouseRuins : bottom

  addExtraDeck UnknownPlacesDeck (top <> bottom')

instance RunMessage TheSecretName where
  runMessage msg s@(TheSecretName attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      enemiesOfTheLodge <- getHasRecord TheInvestigatorsAreEnemiesOfTheLodge
      learnedNothingOfTheLodgesSchemes <- getHasRecord TheInvestigatorsLearnedNothingOfTheLodge'sSchemes
      neverSeenOrHeardFromAgain <- getHasRecord TheInvestigatorsAreNeverSeenOrHeardFromAgain

      flavor do
        setTitle "title"
        p "checkCampaignLog"
        ul do
          li.validate membersOfTheLodge "membersOfTheLodge"
          li.validate enemiesOfTheLodge "enemiesOfTheLodge"
          li.validate learnedNothingOfTheLodgesSchemes "learnedNothingOfTheLodgesSchemes"
          li.validate neverSeenOrHeardFromAgain "neverSeenOrHeardFromAgain"

      let read k = setTitle "title" >> p k
      anyMystic <- selectAny $ InvestigatorWithClass Mystic
      when membersOfTheLodge do
        storyWithChooseOneM' (read "intro1") do
          labeled' "tellTheLodgeOfTheWitches" do
            flavor $ read "intro2"
            record TheInvestigatorsToldTheLodgeAboutTheCoven
            addChaosToken Cultist
          labeled' "lie" do
            flavor do
              setTitle "title"
              p "intro3"
              p.validate anyMystic "intro3Mystic"
              p "intro3Part2"
            record TheInvestigatorsHidTheirKnowledgeOfTheCoven
      when enemiesOfTheLodge $ flavor $ read "intro4"
      when learnedNothingOfTheLodgesSchemes $ flavor $ read "intro5"
      when neverSeenOrHeardFromAgain $ flavor $ read "intro6"
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup TheSecretName attrs $ setupTheSecretName attrs
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing _ | isHardExpert attrs -> do
      push HuntersMove
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist ->
          push
            $ DiscardTopOfEncounterDeck
              iid
              (if isEasyStandard attrs then 3 else 5)
              (toSource attrs)
              Nothing
        Tablet -> do
          selectForMaybeM (enemyIs Enemies.nahab) \nahab -> do
            if isEasyStandard attrs
              then do
                atYourLocation <- nahab <=~> EnemyAt (locationWithInvestigator iid)
                when atYourLocation $ push $ EnemyWillAttack $ enemyAttack nahab attrs iid
              else push $ EnemyWillAttack $ enemyAttack nahab attrs iid
        ElderThing | isEasyStandard attrs -> push HuntersMove
        _ -> pure ()
      pure s
    EnemyDefeated _ cardId _ _ -> do
      isBrownJenkin <- selectAny $ cardIs Enemies.brownJenkin <> CardWithId cardId
      isNahab <- selectAny $ cardIs Enemies.nahab <> CardWithId cardId

      let brownJenkinDefeated = getMetaKeyDefault "brownJenkinDefeated" False attrs || isBrownJenkin
      let nahabDefeated = getMetaKeyDefault "nahabDefeated" False attrs || isNahab

      pure
        . TheSecretName
        $ attrs
        & setMetaKey "brownJenkinDefeated" brownJenkinDefeated
        & setMetaKey "nahabDefeated" nahabDefeated
    ScenarioResolution r -> scope "resolutions" do
      iids <- allInvestigators
      step <- getCurrentActStep
      lead <- getLead
      let
        brownJenkinDefeated = getMetaKeyDefault "brownJenkinDefeated" False attrs
        brownJenkinBonus = if brownJenkinDefeated then toBonus "brownJenkinDefeated" 1 else NoBonus
        nahabDefeated = getMetaKeyDefault "nahabDefeated" False attrs
        nahabBonus = if nahabDefeated then toBonus "nahabDefeated" 1 else NoBonus
        addTheBlackBook = chooseOneM lead do
          labeled' "doNotAddTheBlackBook" nothing
          targets iids \iid -> do
            addCampaignCardToDeck iid DoNotShuffleIn Assets.theBlackBook
            addChaosToken Skull
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R1
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs (brownJenkinBonus <> nahabBonus)
          when (step >= 2) $ recordSetInsert MementosDiscovered [Gilman'sJournal]
          when (step == 3) $ recordSetInsert MementosDiscovered [Keziah'sFormulae]
          when (step >= 2) addTheBlackBook
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "resolution2" 2
          recordSetInsert MementosDiscovered [Gilman'sJournal, Keziah'sFormulae, WornCrucifix]
          addTheBlackBook
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheSecretName <$> liftRunMessage msg attrs
