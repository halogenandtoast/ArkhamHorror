module Arkham.Scenario.Scenarios.DimCarcosa (DimCarcosa (..), dimCarcosa) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Classes
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Card
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (recordSetInsert)
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Scenarios.DimCarcosa.Story
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

instance RunMessage DimCarcosa where
  runMessage msg s@(DimCarcosa attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      openedThePathBelow <- getHasRecord YouOpenedThePathBelow
      story $ if openedThePathBelow then intro1 else intro2
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens

      lead <- getLead
      chooseOne
        lead
        [ Label "Conviction" [RecordCount Conviction 8]
        , Label "Doubt" [RecordCount Doubt 8]
        , Label "Neither" []
        ]

      pathOpened <- sample2 YouOpenedThePathBelow YouOpenedThePathAbove
      record pathOpened

      let token = if pathOpened == YouOpenedThePathBelow then Tablet else ElderThing
      addChaosToken token
      addChaosToken token
      addCampaignCardToDeck lead Enemies.theManInThePallidMask
      pure s
    Setup -> runScenarioSetup DimCarcosa attrs do
      doubt <- getDoubt
      conviction <- getConviction
      lead <- getLead

      if doubt + conviction <= 5
        then push $ SetupStep (toTarget attrs) 1
        else case compare doubt conviction of
          GT -> push $ SetupStep (toTarget attrs) 2
          LT -> push $ SetupStep (toTarget attrs) 3
          EQ ->
            chooseOne
              lead
              [ Label "Use Search For the Stranger (v. II)" [SetupStep (toTarget attrs) 2]
              , Label "Use Search For the Stranger (v. III)" [SetupStep (toTarget attrs) 3]
              ]
    SetupStep (isTarget attrs -> True) n -> runScenarioSetup DimCarcosa attrs do
      gather Set.DimCarcosa
      gather Set.Delusions
      gather Set.CultOfTheYellowSign
      gather Set.InhabitantsOfCarcosa
      gather Set.AgentsOfHastur
      gather Set.StrikingFear

      setAgendaDeck [Agendas.madnessCoils, Agendas.madnessDrowns, Agendas.madnessDies]

      let
        act2 = case n of
          1 -> Acts.searchForTheStrangerV1
          2 -> Acts.searchForTheStrangerV2
          3 -> Acts.searchForTheStrangerV3
          _ -> error $ "Invalid setup step, got: " <> show n
      setActDeck [Acts.inLostCarcosa, act2, Acts.theKingInTatters]

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

      placeAll
        [ bleakPlains
        , ruinsOfCarcosa
        , dimStreets
        , depthsOfDemhe
        , Locations.palaceOfTheKing
        ]

      openedThePathBelow <- getHasRecord YouOpenedThePathBelow
      startAt $ if openedThePathBelow then shoresOfHali else darkSpires

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
      push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      mAction <- getSkillTestAction
      mTarget <- getSkillTestTarget
      case (mAction, mTarget) of
        (Just action, Just (EnemyTarget eid))
          | action `elem` [#fight, #evade] -> do
              isMonsterOrAncientOne <- elem eid <$> select (mapOneOf EnemyWithTrait [Monster, AncientOne])
              pushWhen isMonsterOrAncientOne
                $ LoseActions iid (ChaosTokenEffectSource ElderThing) 1
        _ -> pure ()
      pure s
    ScenarioResolution res -> do
      conviction <- getConviction
      doubt <- getDoubt
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
        NoResolution -> case compare conviction doubt of
          GT -> push R4
          EQ -> push R4
          LT -> push R5
        Resolution 1 -> do
          story resolution1
          eachInvestigator \iid -> sufferTrauma iid 2 2
          allGainXpWithBonus attrs $ toBonus "resolution1" 5
          recordPossessed
          endOfScenario
        Resolution 2 -> do
          story resolution2
          eachInvestigator (`sufferMentalTrauma` 2)
          allGainXpWithBonus attrs $ toBonus "resolution2" 5
          recordPossessed
          endOfScenario
        Resolution 3 -> do
          story resolution3
          eachInvestigator (`sufferPhysicalTrauma` 2)
          allGainXpWithBonus attrs $ toBonus "resolution3" 5
          recordPossessed
          endOfScenario
        Resolution 4 -> do
          story resolution4
          record TheRealmOfCarcosaMergedWithOurOwnAndHasturRulesOverThemBoth
          eachInvestigator drivenInsane
          gameOver
        Resolution 5 -> do
          story resolution5
          record HasturHasYouInHisGrasp
          eachInvestigator drivenInsane
          gameOver
        _ -> error "Unhandled resolution"
      pure s
    _ -> DimCarcosa <$> liftRunMessage msg attrs
