module Arkham.Scenario.Scenarios.MachinationsThroughTime (machinationsThroughTime) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Field
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (assetAt)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Ally, Scientist))

newtype MachinationsThroughTime = MachinationsThroughTime ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machinationsThroughTime :: Difficulty -> MachinationsThroughTime
machinationsThroughTime difficulty =
  sideStory
    MachinationsThroughTime
    "87001"
    "Machinations Through Time"
    difficulty
    [ ".         arkhamGazette .            .            arkhamAdvertiserPresent .                 .           arkhamAdvertiserFuture ."
    , "riverDocksPast .        oMalleysWatchShop riverDocksPresent .             tickTockClubPresent riverDocksFuture .            tickTockClubFuture"
    , ".         miskatonicUniversityPast .  .            miskatonicUniversityPresent .            .           miskatonicUniversityFuture ."
    , ".         childhoodHome .            .            yeOldeMagickShoppe .                      .           corriganIndustries ."
    , ".         .             .            .            tindalos .                                .           .                  ."
    ]

{- FOURMOLU_DISABLE -}
easyTokens, standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
easyTokens =
  [ PlusOne , PlusOne , PlusOne , Zero , Zero , Zero , Zero , MinusOne , MinusOne , MinusOne
  , MinusTwo , MinusTwo , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree
  , MinusFour , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusThree , MinusFour , MinusSix
  , Skull , Skull , Cultist , Tablet , ElderThing , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusOne , MinusTwo , MinusThree , MinusFour , MinusFive , MinusSix
  , MinusEight , Skull , Skull , Cultist , Tablet , ElderThing , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

instance HasChaosTokenValue MachinationsThroughTime where
  getChaosTokenValue iid tokenFace (MachinationsThroughTime attrs) = case tokenFace of
    Skull -> do
      n <- selectCount $ VictoryDisplayCardMatch $ basic #story
      let x = if isEasyStandard attrs then 1 + n else 2 + n
      pure $ ChaosTokenValue Skull (NegativeModifier x)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> do
      scientistHere <-
        selectAny $ AssetWithTrait Scientist <> AssetAt (locationWithInvestigator iid)
      let n
            | scientistHere = if isEasyStandard attrs then 4 else 6
            | otherwise = if isEasyStandard attrs then 2 else 3
      pure $ ChaosTokenValue Tablet (NegativeModifier n)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage MachinationsThroughTime where
  runMessage msg s@(MachinationsThroughTime attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> easyTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> expertTokens
      pure s
    Setup -> runScenarioSetup MachinationsThroughTime attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "corriganIndustries"
          li "startAt"
        li "nobleLegacy"
        li "machination"
        li "plot"
        li "doom"
        unscoped $ li "shuffleRemainder"

      gather Set.MachinationsThroughTime
      gather Set.MachinationsThroughTimeSingleGroup

      setAgendaDeck [Agendas.intoTheVoid, Agendas.timeMarchesOn]
      setActDeck [Acts.walkingThroughTime]

      setAside [Locations.corriganIndustries]

      tindalos <- place Locations.tindalos
      arkhamGazette <- place Locations.arkhamGazette
      oMalleys <- place Locations.oMalleysWatchShop
      riverDocksPast <- placeLabeled "riverDocksPast" Locations.riverDocksPast
      placeLabeled_ "miskatonicUniversityPast" Locations.miskatonicUniversityPast
      childhoodHome <- place Locations.childhoodHome
      arkhamAdvertiserPresent <-
        placeLabeled "arkhamAdvertiserPresent" Locations.arkhamAdvertiserPresent
      placeLabeled_ "tickTockClubPresent" Locations.tickTockClubPresent
      placeLabeled_ "riverDocksPresent" Locations.riverDocksPresent
      miskatonicUniversityPresent <-
        placeLabeled "miskatonicUniversityPresent" Locations.miskatonicUniversityPresent
      _ <- place Locations.yeOldeMagickShoppe
      placeLabeled_ "arkhamAdvertiserFuture" Locations.arkhamAdvertiserFuture
      placeLabeled_ "tickTockClubFuture" Locations.tickTockClubFuture
      placeLabeled_ "riverDocksFuture" Locations.riverDocksFuture
      placeLabeled_ "miskatonicUniversityFuture" Locations.miskatonicUniversityFuture

      startAt tindalos

      -- A Noble Legacy (Past, Present, Future)
      placeStory Stories.aNobleLegacyPast
      assetAt_ Assets.nikolaTesla riverDocksPast
      placeStory Stories.aNobleLegacyPresent
      assetAt_ Assets.ezraGraves arkhamAdvertiserPresent
      placeStory Stories.aNobleLegacyFuture
      setAside [Assets.dimensionalBeamMachine]

      -- Choose one Machination story at random
      machination <-
        sample
          $ Stories.aBitterRivalry
          :| [Stories.redeemAFormerColleague, Stories.uneasyAlliance]
      placeStory machination
      removeEvery
        $ deleteFirst machination
          [Stories.aBitterRivalry, Stories.redeemAFormerColleague, Stories.uneasyAlliance]

      if
        | machination == Stories.aBitterRivalry -> do
            setAside
              [ Assets.thomasCorriganPast
              , Assets.thomasCorriganPresent
              , Assets.maryZielinskiPresent
              , Assets.thomasCorriganFuture
              , Assets.maryZielinskiFuture
              ]
            setAside [Enemies.edwinBennetBitterAdversary]
            assetAt_ Assets.maryZielinskiPast arkhamGazette
        | machination == Stories.redeemAFormerColleague -> do
            setAside
              [ Assets.maryZielinskiPast
              , Assets.thomasCorriganPresent
              , Assets.maryZielinskiPresent
              , Assets.thomasCorriganFuture
              , Assets.maryZielinskiFuture
              ]
            assetAt_ Assets.thomasCorriganPast childhoodHome
            enemyAt_ Enemies.edwinBennetBitterAdversary miskatonicUniversityPresent
        | otherwise -> do
            -- Uneasy Alliance
            setAside
              [ Assets.thomasCorriganPresent
              , Assets.maryZielinskiPresent
              , Assets.thomasCorriganFuture
              , Assets.maryZielinskiFuture
              ]
            assetAt_ Assets.thomasCorriganPast childhoodHome
            assetAt_ Assets.maryZielinskiPast oMalleys
            edwin <- assetAt Assets.edwinBennetAstuteAssociate arkhamGazette
            n <- perPlayer 3
            placeTokens ScenarioSource edwin #clue (max 0 (12 - n))

      -- Choose one Plot story at random
      plot <-
        sample
          $ Stories.anomaliesInSpacetime
          :| [Stories.mobTroubles, Stories.unspeakableAbomination]
      placeStory plot
      removeEvery
        $ deleteFirst plot
          [Stories.anomaliesInSpacetime, Stories.mobTroubles, Stories.unspeakableAbomination]

      if
        | plot == Stories.anomaliesInSpacetime -> do
            removeEvery [Enemies.tyrthrha, Enemies.oldSadieSheldon, Enemies.sheldonGang]
            anomalies <- perPlayer 1
            universities <- select $ LocationWithTitle "Miskatonic University"
            for_ universities \lid -> placeTokens ScenarioSource lid #horror anomalies
        | plot == Stories.mobTroubles -> do
            setAside
              [ Enemies.oldSadieSheldon
              , Enemies.sheldonGang
              , Enemies.sheldonGang
              , Enemies.sheldonGang
              ]
            removeEvery [Enemies.tyrthrha]
            eachInvestigator \iid -> gainResources iid ScenarioSource 2
        | otherwise -> do
            -- Unspeakable Abomination
            setAside [Enemies.tyrthrha]
            removeEvery [Enemies.oldSadieSheldon, Enemies.sheldonGang]

      -- Doom adjustments
      doomCount <- perPlayer 1
      let
        difficultyDoom = case attrs.difficulty of
          Easy -> -1
          Standard -> 0
          Hard -> 1
          Expert -> 2
      placeDoomOnAgenda (max 0 (doomCount + difficultyDoom))
    ResolveChaosToken _ Cultist iid -> do
      atTindalos <- iid <=~> InvestigatorAt (locationIs Locations.tindalos)
      when atTindalos failSkillTest
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == ElderThing) do
        allies <- select $ AssetWithTrait Ally <> AssetAt (locationWithInvestigator iid)
        when (notNull allies) do
          chooseTargetM iid allies \ally -> do
            chooseOneM iid $ withI18n do
              countVar 1 $ labeled' "dealDamage" $ dealAssetDamage ally ElderThing 1
              countVar 1 $ labeled' "dealHorror" $ dealAssetHorror ally ElderThing 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          edwinAsset <- selectAny $ assetIs Assets.edwinBennetAstuteAssociate
          edwinEnemy <- selectAny $ enemyIs Enemies.edwinBennetBitterAdversary
          resolution "noResolution"
          if
            | edwinAsset -> push R2
            | edwinEnemy -> push R3
            | otherwise -> push R4
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          rewardAssets s
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          rewardAssets s
          endOfScenario
        Resolution 3 -> do
          resolution "resolution3"
          eachInvestigator drivenInsane
          endOfScenario
        Resolution 4 -> do
          resolutionWithXp "resolution4" $ allGainXp' attrs
          rewardAssets s
          endOfScenario
        _ -> throw $ UnknownResolution r
      pure s
    _ -> MachinationsThroughTime <$> liftRunMessage msg attrs

rewardAssets :: ReverseQueue m => MachinationsThroughTime -> m ()
rewardAssets (MachinationsThroughTime _attrs) = do
  lead <- getLead
  investigators <- allInvestigators
  for_ [Assets.nikolaTesla, Assets.ezraGraves, Assets.dimensionalBeamMachine] \def -> do
    minPlay <- selectOne $ assetIs def
    for_ minPlay \aid -> do
      card <- field Field.AssetCard aid
      chooseOneM lead $ withI18n do
        labeled' "skip" nothing
        questionLabeledCard def
        portraits investigators \iid -> do
          removeFromGame aid
          addCampaignCardToDeck iid ShuffleIn card
