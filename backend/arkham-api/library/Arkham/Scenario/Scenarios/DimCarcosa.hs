module Arkham.Scenario.Scenarios.DimCarcosa (setupDimCarcosa, dimCarcosa, DimCarcosa (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.Card (genCards)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Card
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Token
import Arkham.Trait (Trait (AncientOne, Monster))
import Arkham.Treachery.Cards qualified as Treacheries

newtype DimCarcosa = DimCarcosa ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimCarcosa :: Difficulty -> DimCarcosa
dimCarcosa difficulty =
  scenario
    DimCarcosa
    "03316"
    "Dim Carcosa"
    difficulty
    [ ".          darkSpires      ."
    , ".          depthsOfDemhe   ."
    , "dimStreets palaceOfTheKing ruinsOfCarcosa"
    , ".          bleakPlains     ."
    , ".          shoresOfHali    ."
    ]

instance HasModifiersFor DimCarcosa where
  getModifiersFor (DimCarcosa a) = do
    knowTheSecret <- remembered KnowTheSecret
    modifySelectWhen a (not knowTheSecret) (EnemyWithTitle "Hastur") [CannotBeDefeated]
    modifySelectWith a Anyone setActiveDuringSetup [CanOnlyBeDefeatedByDamage]

instance HasChaosTokenValue DimCarcosa where
  getChaosTokenValue iid chaosTokenFace (DimCarcosa attrs) = case chaosTokenFace of
    Skull -> do
      remainingSanity <- field InvestigatorRemainingSanity iid
      horror <- field InvestigatorHorror iid
      pure $ toChaosTokenValue attrs Skull (if remainingSanity == 0 then 4 else 2) horror
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
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
  , MinusFive
  , Skull
  , Skull
  , Skull
  , Cultist
  , Cultist
  , AutoFail
  , ElderSign
  ]

setupDimCarcosa :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupDimCarcosa attrs = do
  doubt <- getDoubt
  conviction <- getConviction
  tallies <- getRecordCount ChasingTheStranger
  openedThePathBelow <- getHasRecord YouOpenedThePathBelow

  setup do
    ul do
      li "gatherSets"
      li.nested "act2.instructions" do
        li.validate (doubt + conviction <= 5) "act2.v1"
        li.validate (doubt + conviction >= 6 && doubt > conviction) "act2.v2"
        li.validate (doubt + conviction >= 6 && doubt < conviction) "act2.v3"
        li.validate (doubt + conviction >= 6 && doubt == conviction) "act2.choose"
      li "chooseLocations"
      li.nested "placeLocations.instructions" do
        li.validate openedThePathBelow "placeLocations.startAtShoresOfHali"
        li.validate (not openedThePathBelow) "placeLocations.startAtDarkSpires"
      li "theManInThePallidMask"
      li "setAside"
      li.nested "chasingTheStranger.instructions" do
        li.validate (tallies <= 2) "chasingTheStranger.twoOrFewer"
        li.validate (tallies >= 3 && tallies <= 5) "chasingTheStranger.threeToFive"
        li.validate (tallies >= 6 && tallies <= 8) "chasingTheStranger.sixToEight"
        li.validate (tallies >= 9) "chasingTheStranger.nineOrMore"
      li "takeHorror"
      unscoped $ li "shuffleRemainder"

  whenReturnTo $ gather Set.ReturnToDimCarcosa
  gather Set.DimCarcosa
  gather Set.Delusions `orWhenReturnTo` gather Set.MaddeningDelusions
  gather Set.CultOfTheYellowSign
  gather Set.InhabitantsOfCarcosa
  gather Set.AgentsOfHastur `orWhenReturnTo` gather Set.HastursEnvoys
  gather Set.StrikingFear `orWhenReturnTo` gather Set.NeuroticFear

  setAgendaDeck [Agendas.madnessCoils, Agendas.madnessDrowns, Agendas.madnessDies]

  if doubt + conviction <= 5
    then doStep 1 Setup
    else case compare doubt conviction of
      GT -> doStep 2 Setup
      LT -> doStep 3 Setup
      EQ -> leadChooseOneM do
        labeled' "useV2" $ doStep 2 Setup
        labeled' "useV3" $ doStep 3 Setup

  shoresOfHali <- place Locations.shoresOfHali
  darkSpires <- place Locations.darkSpires

  (bleakPlains, setAsideBleakPlains) <-
    sampleWithRest $ Locations.bleakPlainsBleakDesolation :| [Locations.bleakPlainsStarsOfAldebaran]
  (ruinsOfCarcosa, setAsideRuinsOfCarcosa) <-
    sampleWithRest
      $ Locations.ruinsOfCarcosaTheCoffin
      :| [ Locations.ruinsOfCarcosaInhabitantOfCarcosa
         , Locations.ruinsOfCarcosaAMomentsRest
         ]
  (dimStreets, setAsideDimStreets) <-
    sampleWithRest
      $ Locations.dimStreetsMappingTheStreets
      :| [ Locations.dimStreetsTheArchway
         , Locations.dimStreetsTheKingsParade
         ]
  (depthsOfDemhe, setAsideDepthsOfDemhe) <-
    sampleWithRest
      $ Locations.depthsOfDemheStepsOfThePalace
      :| [Locations.depthsOfDemheTheHeightOfTheDepths]

  isReturnTo <- getIsReturnTo

  placeAll
    [ bleakPlains
    , ruinsOfCarcosa
    , dimStreets
    , depthsOfDemhe
    , if isReturnTo then Locations.returnToPalaceOfTheKing else Locations.palaceOfTheKing
    ]

  startAt $ if openedThePathBelow then shoresOfHali else darkSpires

  when (tallies < 9) do
    placeDoomOnAgenda
      $ if
        | tallies <= 2 -> 3
        | tallies <= 5 -> 2
        | otherwise -> 1

  eachInvestigator \iid ->
    placeTokens attrs iid #horror
      =<< fieldMap InvestigatorSanity (`div` 2) iid

  theManInThePallidMask <- getCampaignStoryCard Enemies.theManInThePallidMask
  push $ RemoveFromBearersDeckOrDiscard theManInThePallidMask

  setAside
    $ Enemies.theManInThePallidMask
    : [ Enemies.hasturTheKingInYellow
      , Enemies.hasturLordOfCarcosa
      , Enemies.hasturTheTatteredKing
      , Enemies.beastOfAldebaran
      ]
      <> setAsideBleakPlains
      <> setAsideRuinsOfCarcosa
      <> setAsideDimStreets
      <> setAsideDepthsOfDemhe

  whenReturnTo do
    setAside [Locations.recessesOfYourOwnMind, Locations.theThroneRoom, Locations.stageOfTheWardTheatre]
    addAdditionalReferences ["52059b"]

instance RunMessage DimCarcosa where
  runMessage msg s@(DimCarcosa attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      openedThePathBelow <- getHasRecord YouOpenedThePathBelow
      flavor do
        h "title"
        p.validate openedThePathBelow "readIntro1"
        p.validate (not openedThePathBelow) "readIntro2"

      flavor do
        h "title"
        p $ if openedThePathBelow then "intro1" else "intro2"
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens

      leadChooseOneM do
        popScope $ labeled' "conviction" $ markConvictionN 8
        popScope $ labeled' "doubt" $ markDoubtN 8
        unscoped $ labeled' "neither" nothing

      pathOpened <- sample2 YouOpenedThePathBelow YouOpenedThePathAbove
      record pathOpened

      twice $ addChaosToken $ if pathOpened == YouOpenedThePathBelow then Tablet else ElderThing
      lead <- getLead
      addCampaignCardToDeck lead ShuffleIn Enemies.theManInThePallidMask
      pure s
    Setup -> runScenarioSetup DimCarcosa attrs $ setupDimCarcosa attrs
    DoStep n Setup -> do
      let
        act2 = case n of
          1 -> Acts.searchForTheStrangerV1
          2 -> Acts.searchForTheStrangerV2
          3 -> Acts.searchForTheStrangerV3
          _ -> error $ "Invalid setup step, got: " <> show n
      push SetActDeck
      acts <- genCards [Acts.inLostCarcosa, act2, Acts.theKingInTatters]
      pure $ DimCarcosa $ attrs & actStackL %~ insertMap 1 acts
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == Cultist) do
        assignHorror iid Cultist $ if isEasyStandard attrs then 1 else 2
      when (token.face == Tablet) $ do
        hasturInPlay <- selectAny $ EnemyWithTitle "Hastur"
        when hasturInPlay $ do
          mlid <- field InvestigatorLocation iid
          for_ mlid $ \lid -> placeTokens attrs lid Clue 1
      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      void $ runMaybeT do
        action <- MaybeT getSkillTestAction
        guard $ action `elem` [#fight, #evade]
        eid <- MaybeT getSkillTestTargetedEnemy
        liftGuardM $ elem eid <$> select (mapOneOf EnemyWithTrait [Monster, AncientOne])
        lift $ loseActions iid ElderThing 1
      pure s
    ScenarioResolution res -> scope "resolutions" do
      conviction <- getConviction
      doubt <- getDoubt
      case res of
        NoResolution -> do
          flavor do
            h "noResolution"
            p.validate (conviction >= doubt) "goToResolution4"
            p.validate (conviction < doubt) "goToResolution5"
          do_ $ if conviction >= doubt then R4 else R5
        _ -> do_ msg
      pure s
    Do (ScenarioResolution res) -> scope "resolutions" do
      possessed <-
        select
          $ InvestigatorWithTreacheryInHand
          $ mapOneOf
            treacheryIs
            [ Treacheries.possessionMurderous
            , Treacheries.possessionTraitorous
            , Treacheries.possessionTorturous
            ]
      let recordPossessed = recordSetInsert Possessed (map unInvestigatorId possessed)
      case res of
        Resolution 1 -> do
          record TheInvestigatorsPreventedHasturFromEscapingHisPrison
          eachInvestigator \iid -> sufferTrauma iid 2 2
          recordPossessed
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "self" 5
          endOfScenario
        Resolution 2 -> do
          record TheInvestigatorsPreventedHasturFromEscapingHisPrison
          eachInvestigator (`sufferMentalTrauma` 2)
          recordPossessed
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "truth" 5
          endOfScenario
        Resolution 3 -> do
          record TheInvestigatorsPreventedHasturFromEscapingHisPrison
          eachInvestigator (`sufferPhysicalTrauma` 2)
          recordPossessed
          resolutionWithXp "resolution3" $ allGainXpWithBonus' attrs $ toBonus "truth" 5
          endOfScenario
        Resolution 4 -> do
          record TheRealmOfCarcosaMergedWithOurOwnAndHasturRulesOverThemBoth
          eachInvestigator drivenInsane
          resolution "resolution4"
          gameOver
        Resolution 5 -> do
          record HasturHasYouInHisGrasp
          eachInvestigator drivenInsane
          resolution "resolution5"
          gameOver
        _ -> error "Unhandled resolution"
      pure s
    _ -> DimCarcosa <$> liftRunMessage msg attrs
