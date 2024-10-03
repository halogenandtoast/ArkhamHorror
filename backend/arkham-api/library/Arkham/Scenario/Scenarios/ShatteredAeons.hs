module Arkham.Scenario.Scenarios.ShatteredAeons (ShatteredAeons (..), shatteredAeons) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetCardsUnderneath))
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Card
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (checkWhen, setupModifier)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (victoryDisplayL)
import Arkham.Scenarios.ShatteredAeons.Helpers
import Arkham.Scenarios.ShatteredAeons.Story
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window

newtype ShatteredAeons = ShatteredAeons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredAeons :: Difficulty -> ShatteredAeons
shatteredAeons difficulty =
  scenario
    ShatteredAeons
    "04314"
    "Shattered Aeons"
    difficulty
    [ "shoresOfRlyeh   betweenWorlds1 atlantis    ruinsOfNewYork ."
    , "shoresOfRlyeh   betweenWorlds1 atlantis    ruinsOfNewYork valusia"
    , "cityOfTheUnseen nexusOfNkai    .           aPocketInTime  valusia"
    , "cityOfTheUnseen nexusOfNlai    .           aPocketInTime  pnakotus"
    , "yuggoth         betweenWorlds2 mu          plateauOfLeng  pnakotus"
    , "yuggoth         betweenWorlds2 mu          plateauOfLeng  ."
    ]

instance HasChaosTokenValue ShatteredAeons where
  getChaosTokenValue iid chaosTokenFace (ShatteredAeons attrs) = case chaosTokenFace of
    Skull -> do
      atRelicsLocation <-
        selectAny
          $ assetIs Assets.relicOfAgesUnleashTheTimestream
          <> AssetAt
            (locationWithInvestigator iid)
      pure
        $ if atRelicsLocation
          then toChaosTokenValue attrs Skull 4 5
          else toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> do
      poisoned <- getIsPoisoned iid
      pure
        $ if poisoned
          then ChaosTokenValue Tablet AutoFailModifier
          else toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFour
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecorded = setFromList [TheBraziersAreLit, TheRelicIsMissing]
    }

instance RunMessage ShatteredAeons where
  runMessage msg s@(ShatteredAeons attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      braziersLit <- getHasRecord TheBraziersAreLit
      doStep (if braziersLit then 1 else 2) PreScenarioSetup
      pure s
    DoStep 1 PreScenarioSetup -> do
      story intro1
      doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> do
      story intro2
      eachInvestigator \iid -> setupModifier attrs iid (StartingHand (-2))
      doStep 3 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> do
      story intro3
      foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic
      doStep (if foundTheMissingRelic then 4 else 5) PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> do
      story intro4
      pure s
    DoStep 5 PreScenarioSetup -> do
      story intro5
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      lead <- getLead
      chooseOneM lead do
        labeled "Ichtaca is set against you. Add 3 {tablet} tokens to the chaos bag." do
          record IchtacaIsSetAgainstYou
          addChaosToken Tablet
          addChaosToken Tablet
          addChaosToken Tablet
        labeled "Alejandro is set against you. Add 3 {cultist} tokens to the chaos bag." do
          record AlejandroIsSetAgainstYou
          addChaosToken Cultist
          addChaosToken Cultist
          addChaosToken Cultist
        labeled
          "Ichtaca is set against you. Alejandro is set against you. Add 2 {elderThing} tokens to the chaos bag. Choose this option for the ultimate challenge."
          do
            record IchtacaIsSetAgainstYou
            record AlejandroIsSetAgainstYou
            addChaosToken ElderThing
            addChaosToken ElderThing
      pure . ShatteredAeons $ attrs & standaloneCampaignLogL .~ standaloneCampaignLog
    Setup -> runScenarioSetup ShatteredAeons attrs do
      gather Set.ShatteredAeons
      gather Set.PnakoticBrotherhood
      gather Set.TemporalFlux
      gather Set.AncientEvils

      tokens <- getBagChaosTokens
      let tokenCount face = count ((== face) . (.face)) tokens
      case compare (tokenCount Cultist) (tokenCount Tablet) of
        GT -> gather Set.DarkCult
        LT -> gather Set.AgentsOfYig
        EQ -> do
          gather Set.DarkCult
          gather Set.AgentsOfYig

      setActDeck
        [ Acts.worldsBeyond
        , Acts.searchForTheBrotherhood
        , Acts.theYithianRelic
        , Acts.mendTheShatter
        ]

      setAgendaDeck
        [ Agendas.threadsOfTime
        , Agendas.pendulousThreads
        , Agendas.snappedThreads
        ]

      startAt =<< place Locations.nexusOfNKai

      setAside
        [ Assets.relicOfAgesUnleashTheTimestream
        , Enemies.ichtacaScionOfYig
        , Enemies.alejandroVela
        , Enemies.formlessSpawn
        , Locations.aPocketInTime
        , Locations.ruinsOfNewYork
        , Locations.mu
        , Locations.atlantis
        , Locations.pnakotus
        , Locations.valusia
        , Locations.plateauOfLeng
        , Acts.paradiseLost
        , Acts.timelock
        ]

      addExtraDeck ExplorationDeck
        =<< shuffle
          [ Locations.yuggoth
          , Locations.shoresOfRlyeh
          , Locations.cityOfTheUnseen
          , Treacheries.wrackedByTime
          , Treacheries.betweenWorlds
          , Treacheries.ancientEvils
          ]

      yigsFury <- getRecordCount YigsFury
      leadDeck <- fieldMap InvestigatorDeck unDeck =<< getLead
      let cardsToAddToVictory = map PlayerCard $ take (yigsFury `div` 10) leadDeck
      pushAll $ map (RemovePlayerCardFromGame False) cardsToAddToVictory
      placeInVictory $ map VengeanceCard cardsToAddToVictory
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n | n < 1 -> do
      when (chaosTokenFace token == Cultist) $ do
        if isEasyStandard attrs
          then do
            cultists <- select $ NearestEnemyTo iid $ EnemyWithTrait Trait.Cultist
            chooseTargetM iid cultists \cultist -> placeDoom Cultist cultist 1
          else do
            cultists <- select $ EnemyWithTrait Trait.Cultist
            for_ cultists \cultist -> placeDoom Cultist cultist 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == Cultist) $ do
        if isEasyStandard attrs
          then do
            cultists <- select $ NearestEnemyTo iid $ EnemyWithTrait Trait.Cultist
            chooseTargetM iid cultists \cultist -> placeDoom Cultist cultist 1
          else do
            cultists <- select $ EnemyWithTrait Trait.Cultist
            for_ cultists \cultist -> placeDoom Cultist cultist 1
      when (token.face == Tablet && isHardExpert attrs) $ do
        isPoisoned <- getIsPoisoned iid
        unless isPoisoned $ do
          poisoned <- getSetAsidePoisoned
          push $ CreateWeaknessInThreatArea poisoned iid
      when (token.face == ElderThing) $ do
        let mHex = find (`cardMatch` CardWithTrait Trait.Hex) attrs.discard
        for_ mHex $ \hex -> shuffleCardsIntoDeck ExplorationDeck [EncounterCard hex]
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      let mHex = find (`cardMatch` CardWithTrait Trait.Hex) attrs.discard
      modifiers <- getModifiers (ChaosTokenFaceTarget ElderThing)
      when (RevealAnotherChaosToken `elem` modifiers) $ drawAnotherChaosToken iid
      for_ mHex $ \hex -> shuffleCardsIntoDeck ExplorationDeck [EncounterCard hex]
      pure s
    ResolveChaosToken _ face iid -> do
      modifiers <- getModifiers (ChaosTokenFaceTarget face)
      when (RevealAnotherChaosToken `elem` modifiers) $ drawAnotherChaosToken iid
      ShatteredAeons <$> liftRunMessage msg attrs
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      push $ Do msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ScenarioResolution resolution -> do
      case resolution of
        NoResolution -> do
          push R4
          pure s
        Resolution 1 -> do
          mrelic <- selectOne $ AssetWithTitle "Relic of Ages"
          locations <- case mrelic of
            Nothing -> pure []
            Just relic -> fieldMapM AssetCardsUnderneath (filterM getHasVictoryPoints) relic
          let attrs' = attrs & victoryDisplayL %~ (locations <>)
          story resolution1
          record TheInvestigatorsMendedTheTearInTheFabricOfTime
          eachInvestigator \iid -> sufferTrauma iid 2 2
          allGainXpWithBonus attrs' $ toBonus "resolution1" 5
          endOfScenario
          pure $ ShatteredAeons attrs'
        Resolution 2 -> do
          story resolution2
          record TheInvestigatorsSavedTheCivilizationOfTheSerpents
          endOfScenario
          pure s
        Resolution 3 -> do
          story resolution3
          record TheInvestigatorsSavedTheCivilizationOfTheYithians
          endOfScenario
          pure s
        Resolution 4 -> do
          story resolution4
          record TheFabricOfTimeIsUnwoven
          eachInvestigator drivenInsane
          gameOver
          pure s
        Resolution 5 -> do
          mrelic <- selectOne $ AssetWithTitle "Relic of Ages"
          locations <- case mrelic of
            Nothing -> pure []
            Just relic -> fieldMapM AssetCardsUnderneath (filterM getHasVictoryPoints) relic
          let attrs' = attrs & victoryDisplayL %~ (locations <>)
          story resolution5
          record TheInvestigatorsTurnedBackTime
          allGainXp attrs
          endOfScenarioThen EpilogueStep
          pure $ ShatteredAeons attrs'
        _ -> error "invalid resolution"
    _ -> ShatteredAeons <$> liftRunMessage msg attrs
