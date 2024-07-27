module Arkham.Scenario.Scenarios.ShatteredAeons (
  ShatteredAeons (..),
  shatteredAeons,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetCardsUnderneath))
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Card
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.ShatteredAeons.Story
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (mkWindow)
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
  runMessage msg s@(ShatteredAeons attrs) = case msg of
    StandaloneSetup -> do
      lead <- getLeadPlayer
      pushAll
        [ SetChaosTokens standaloneChaosTokens
        , chooseOne
            lead
            [ Label
                "Ichtaca is set against you. Add 3 {tablet} tokens to the chaos bag."
                [ Record IchtacaIsSetAgainstYou
                , AddChaosToken Tablet
                , AddChaosToken Tablet
                , AddChaosToken Tablet
                ]
            , Label
                "Alejandro is set against you. Add 3 {cultist} tokens to the chaos bag."
                [ Record AlejandroIsSetAgainstYou
                , AddChaosToken Cultist
                , AddChaosToken Cultist
                , AddChaosToken Cultist
                ]
            , Label
                " Ichtaca is set against you. Alejandro is set against you. Add2 {elderThing} tokens to the chaos bag. Choose this option for the ultimate challenge."
                [ Record IchtacaIsSetAgainstYou
                , Record AlejandroIsSetAgainstYou
                , AddChaosToken ElderThing
                , AddChaosToken ElderThing
                ]
            ]
        ]

      pure
        . ShatteredAeons
        $ attrs
        & standaloneCampaignLogL
        .~ standaloneCampaignLog
    Setup -> do
      braziersLit <- getHasRecord TheBraziersAreLit
      foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic
      yigsFury <- getRecordCount YigsFury

      let
        showIntro1 = braziersLit
        showIntro2 = not braziersLit
        showIntro4 = foundTheMissingRelic
        showIntro5 = not foundTheMissingRelic

      lead <- getLead
      players <- allPlayers
      tokens <- getBagChaosTokens

      leadDeck <- fieldMap InvestigatorDeck unDeck lead

      let
        cultistCount = count ((== Cultist) . chaosTokenFace) tokens
        tabletCount = count ((== Tablet) . chaosTokenFace) tokens
        additionalSets = case compare cultistCount tabletCount of
          GT -> [EncounterSet.DarkCult]
          LT -> [EncounterSet.AgentsOfYig]
          EQ -> [EncounterSet.DarkCult, EncounterSet.AgentsOfYig]
        cardsToAddToVictory =
          map PlayerCard $ take (yigsFury `div` 10) leadDeck

      encounterDeck <-
        buildEncounterDeckExcluding
          [ Enemies.ichtacaScionOfYig
          , Enemies.alejandroVela
          , Enemies.formlessSpawn
          , Locations.yuggoth
          , Locations.shoresOfRlyeh
          , Locations.cityOfTheUnseen
          , Locations.aPocketInTime
          , Locations.ruinsOfNewYork
          , Locations.mu
          , Locations.atlantis
          , Locations.pnakotus
          , Locations.valusia
          , Locations.plateauOfLeng
          ]
          $ [ EncounterSet.ShatteredAeons
            , EncounterSet.PnakoticBrotherhood
            , EncounterSet.TemporalFlux
            , EncounterSet.AncientEvils
            ]
          <> additionalSets

      (nexusOfNKaiId, placeNexusOfNKai) <-
        placeLocationCard
          Locations.nexusOfNKai

      let
        encounterDeck' =
          removeEachFromDeck
            encounterDeck
            [ Treacheries.wrackedByTime
            , Treacheries.betweenWorlds
            , Treacheries.ancientEvils
            ]

      explorationDeck <-
        shuffleM
          =<< genCards
            [ Locations.yuggoth
            , Locations.shoresOfRlyeh
            , Locations.cityOfTheUnseen
            , Treacheries.wrackedByTime
            , Treacheries.betweenWorlds
            , Treacheries.ancientEvils
            ]

      setAsideCards <-
        genCards
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

      pushAll
        $ [story players intro1 | showIntro1]
        <> [story players intro2 | showIntro2]
        <> [story players intro3]
        <> [story players intro4 | showIntro4]
        <> [story players intro5 | showIntro5]
        <> [ SetEncounterDeck encounterDeck'
           , SetAgendaDeck
           , SetActDeck
           , placeNexusOfNKai
           , MoveAllTo (toSource attrs) nexusOfNKaiId
           ]
        <> map (RemovePlayerCardFromGame False) cardsToAddToVictory

      acts <-
        genCards
          [ Acts.worldsBeyond
          , Acts.searchForTheBrotherhood
          , Acts.theYithianRelic
          , Acts.mendTheShatter
          ]

      agendas <-
        genCards
          [ Agendas.threadsOfTime
          , Agendas.pendulousThreads
          , Agendas.snappedThreads
          ]

      ShatteredAeons
        <$> runMessage
          msg
          ( attrs
              & (decksL . at ExplorationDeck ?~ explorationDeck)
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
              & (setAsideCardsL .~ setAsideCards)
              & (victoryDisplayL .~ map VengeanceCard cardsToAddToVictory)
          )
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n | n < 1 -> do
      when (chaosTokenFace token == Cultist) $ do
        if isEasyStandard attrs
          then do
            player <- getPlayer iid
            cultists <- select $ NearestEnemy $ EnemyWithTrait Trait.Cultist
            unless (null cultists)
              $ push
              $ chooseOne
                player
                [ targetLabel cultist [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget cultist) Doom 1]
                | cultist <- cultists
                ]
          else do
            cultists <- select $ EnemyWithTrait Trait.Cultist
            pushAll
              $ [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget cultist) Doom 1 | cultist <- cultists]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (chaosTokenFace token == Cultist) $ do
        if isEasyStandard attrs
          then do
            cultists <- select $ NearestEnemy $ EnemyWithTrait Trait.Cultist
            unless (null cultists) $ do
              player <- getPlayer iid
              push
                $ chooseOne
                  player
                  [ targetLabel cultist [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget cultist) Doom 1]
                  | cultist <- cultists
                  ]
          else do
            cultists <- select $ EnemyWithTrait Trait.Cultist
            pushAll
              $ [PlaceTokens (ChaosTokenEffectSource Cultist) (EnemyTarget cultist) Doom 1 | cultist <- cultists]
      when (chaosTokenFace token == Tablet && isHardExpert attrs) $ do
        isPoisoned <- getIsPoisoned iid
        unless isPoisoned $ do
          poisoned <- getSetAsidePoisoned
          push $ CreateWeaknessInThreatArea poisoned iid
      when (chaosTokenFace token == ElderThing) $ do
        let
          mHex =
            find (`cardMatch` CardWithTrait Trait.Hex) (scenarioDiscard attrs)
        for_ mHex $ \hex -> do
          push
            $ ShuffleCardsIntoDeck
              (Deck.ScenarioDeckByKey ExplorationDeck)
              [EncounterCard hex]
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      let
        mHex =
          find (`cardMatch` CardWithTrait Trait.Hex) (scenarioDiscard attrs)
      modifiers <- getModifiers (ChaosTokenFaceTarget ElderThing)
      when (RevealAnotherChaosToken `elem` modifiers) $ push $ DrawAnotherChaosToken iid
      for_ mHex $ \hex -> do
        push
          $ ShuffleCardsIntoDeck
            (Deck.ScenarioDeckByKey ExplorationDeck)
            [EncounterCard hex]
      pure s
    ResolveChaosToken _ face iid -> do
      modifiers <- getModifiers (ChaosTokenFaceTarget face)
      when (RevealAnotherChaosToken `elem` modifiers) $ push $ DrawAnotherChaosToken iid
      ShatteredAeons <$> runMessage msg attrs
    Explore iid _ _ -> do
      windowMsg <- checkWindows [mkWindow Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ScenarioResolution resolution -> do
      players <- allPlayers
      iids <- allInvestigatorIds
      case resolution of
        NoResolution -> do
          push $ ScenarioResolution $ Resolution 4
          pure s
        Resolution 1 -> do
          mrelic <- selectOne $ AssetWithTitle "Relic of Ages"
          locations <- case mrelic of
            Nothing -> pure []
            Just relic ->
              fieldMapM AssetCardsUnderneath (filterM getHasVictoryPoints) relic
          bonus <- sum . catMaybes <$> traverse getVictoryPoints locations
          xp <- toGainXp attrs $ getXpWithBonus (5 + bonus)
          pushAll
            $ story players resolution1
            : Record TheInvestigatorsMendedTheTearInTheFabricOfTime
            : [SufferTrauma iid 2 2 | iid <- iids]
              <> xp
              <> [EndOfGame Nothing]
          pure . ShatteredAeons $ attrs & victoryDisplayL %~ (locations <>)
        Resolution 2 -> do
          pushAll
            [ story players resolution2
            , Record TheInvestigatorsSavedTheCivilizationOfTheSerpents
            , EndOfGame Nothing
            ]
          pure s
        Resolution 3 -> do
          pushAll
            [ story players resolution3
            , Record TheInvestigatorsSavedTheCivilizationOfTheYithians
            , EndOfGame Nothing
            ]
          pure s
        Resolution 4 -> do
          pushAll
            $ story players resolution4
            : Record TheFabricOfTimeIsUnwoven
            : map DrivenInsane iids
              <> [GameOver]
          pure s
        Resolution 5 -> do
          mrelic <- selectOne $ AssetWithTitle "Relic of Ages"
          locations <- case mrelic of
            Nothing -> pure []
            Just relic ->
              fieldMapM AssetCardsUnderneath (filterM getHasVictoryPoints) relic
          bonus <- sum . catMaybes <$> traverse getVictoryPoints locations
          xp <- toGainXp attrs $ getXpWithBonus bonus
          pushAll
            $ story players resolution5
            : Record TheInvestigatorsTurnedBackTime
            : xp
              <> [EndOfGame $ Just EpilogueStep]
          pure s
        _ -> error "invalid resolution"
    _ -> ShatteredAeons <$> runMessage msg attrs
