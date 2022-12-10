module Arkham.Scenario.Scenarios.ShatteredAeons
  ( ShatteredAeons(..)
  , shatteredAeons
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.ShatteredAeons.Story
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window
import Arkham.Window (Window(..))

newtype ShatteredAeons = ShatteredAeons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredAeons :: Difficulty -> ShatteredAeons
shatteredAeons difficulty = scenario
  ShatteredAeons
  "04314"
  "Shattered Aeons"
  difficulty
  [ "shoresOfRlyeh   atlantis    ruinsOfNewYork ."
  , "shoresOfRlyeh   atlantis    ruinsOfNewYork valusia"
  , "cityOfTheUnseen nexusOfNkai aPocketInTime  valusia"
  , "cityOfTheUnseen nexusOfNlai aPocketInTime  pnakotus"
  , "yuggoth         mu          plateauOfLeng  pnakotus"
  , "yuggoth         mu          plateauOfLeng  ."
  ]

instance HasTokenValue ShatteredAeons where
  getTokenValue iid tokenFace (ShatteredAeons attrs) = case tokenFace of
    Skull -> do
      atRelicsLocation <-
        selectAny $ assetIs Assets.relicOfAgesUnleashTheTimestream <> AssetAt
          (locationWithInvestigator iid)
      pure $ if atRelicsLocation
        then toTokenValue attrs Skull 4 5
        else toTokenValue attrs Skull 2 3
    Cultist -> pure $ toTokenValue attrs Cultist 2 3
    Tablet -> do
      poisoned <- getIsPoisoned iid
      pure $ if poisoned
        then TokenValue Tablet AutoFailModifier
        else toTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toTokenValue attrs ElderThing 2 3
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
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
standaloneCampaignLog = mkCampaignLog
  { campaignLogRecorded = setFromList [TheBraziersAreLit, TheRelicIsMissing]
  }

instance RunMessage ShatteredAeons where
  runMessage msg s@(ShatteredAeons attrs) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ push $ SetTokens standaloneTokens
      pure s
    StandaloneSetup -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne
        leadInvestigatorId
        [ Label
          "Ichtaca is set against you. Add 3 {tablet} tokens to the chaos bag."
          [ Record IchtacaIsSetAgainstYou
          , AddToken Tablet
          , AddToken Tablet
          , AddToken Tablet
          ]
        , Label
          "Alejandro is set against you. Add 3 {cultist} tokens to the chaos bag."
          [ Record AlejandroIsSetAgainstYou
          , AddToken Cultist
          , AddToken Cultist
          , AddToken Cultist
          ]
        , Label
          " Ichtaca is set against you. Alejandro is set against you. Add2 {elderThing} tokens to the chaos bag. Choose this option for the ultimate challenge."
          [ Record IchtacaIsSetAgainstYou
          , Record AlejandroIsSetAgainstYou
          , AddToken ElderThing
          , AddToken ElderThing
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

      lead <- getLeadInvestigatorId
      investigators <- allInvestigatorIds
      tokens <- getTokensInBag

      leadDeck <- fieldMap InvestigatorDeck unDeck lead

      let
        cultistCount = count ((== Cultist) . tokenFace) tokens
        tabletCount = count ((== Tablet) . tokenFace) tokens
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

      nexusOfNKai <- genCard Locations.nexusOfNKai

      let
        encounterDeck' = removeEachFromDeck
          encounterDeck
          [ Treacheries.wrackedByTime
          , Treacheries.betweenWorlds
          , Treacheries.ancientEvils
          ]

      explorationDeck <- shuffleM =<< traverse
        genCard
        [ Locations.yuggoth
        , Locations.shoresOfRlyeh
        , Locations.cityOfTheUnseen
        , Treacheries.wrackedByTime
        , Treacheries.betweenWorlds
        , Treacheries.ancientEvils
        ]

      setAsideCards <- traverse
        genCard
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
        $ [ story investigators intro1 | showIntro1 ]
        <> [ story investigators intro2 | showIntro2 ]
        <> [story investigators intro3]
        <> [ story investigators intro4 | showIntro4 ]
        <> [ story investigators intro5 | showIntro5 ]
        <> [ SetEncounterDeck encounterDeck'
           , SetAgendaDeck
           , SetActDeck
           , PlaceLocation nexusOfNKai
           , MoveAllTo (toSource attrs) (toLocationId nexusOfNKai)
           ]
        <> map RemovePlayerCardFromGame cardsToAddToVictory

      ShatteredAeons <$> runMessage
        msg
        (attrs
        & (decksL . at ExplorationDeck ?~ explorationDeck)
        & (agendaStackL
          . at 1
          ?~ [ Agendas.threadsOfTime
             , Agendas.pendolousThreads
             , Agendas.snappedThreads
             ]
          )
        & (actStackL
          . at 1
          ?~ [ Acts.worldsBeyond
             , Acts.searchForTheBrotherhood
             , Acts.theYithianRelic
             , Acts.mendTheShatter
             ]
          )
        & (setAsideCardsL .~ setAsideCards)
        & (victoryDisplayL .~ map VengeanceCard cardsToAddToVictory)
        )
    PassedSkillTest iid _ _ (TokenTarget token) _ n | n < 1 -> do
      when (tokenFace token == Cultist) $ do
        if isEasyStandard attrs
          then do
            cultists <- selectList $ NearestEnemy $ EnemyWithTrait Trait.Cultist
            unless (null cultists) $ push $ chooseOne
              iid
              [ targetLabel cultist [PlaceDoom (EnemyTarget cultist) 1]
              | cultist <- cultists
              ]
          else do
            cultists <- selectList $ EnemyWithTrait Trait.Cultist
            pushAll
              $ [ PlaceDoom (EnemyTarget cultist) 1 | cultist <- cultists ]
      pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> do
      when (tokenFace token == Cultist) $ do
        if isEasyStandard attrs
          then do
            cultists <- selectList $ NearestEnemy $ EnemyWithTrait Trait.Cultist
            unless (null cultists) $ push $ chooseOne
              iid
              [ targetLabel cultist [PlaceDoom (EnemyTarget cultist) 1]
              | cultist <- cultists
              ]
          else do
            cultists <- selectList $ EnemyWithTrait Trait.Cultist
            pushAll
              $ [ PlaceDoom (EnemyTarget cultist) 1 | cultist <- cultists ]
      when (tokenFace token == Tablet && isHardExpert attrs) $ do
        isPoisoned <- getIsPoisoned iid
        unless isPoisoned $ do
          poisoned <- getSetAsidePoisoned
          push $ CreateWeaknessInThreatArea poisoned iid
      when (tokenFace token == ElderThing) $ do
        let
          mHex =
            find (`cardMatch` CardWithTrait Trait.Hex) (scenarioDiscard attrs)
        for_ mHex $ \hex -> do
          pushAll
            [ RemoveFromEncounterDiscard hex
            , ShuffleCardsIntoDeck
              (Deck.ScenarioDeckByKey ExplorationDeck)
              [EncounterCard hex]
            ]
      pure s
    ResolveToken _ ElderThing _ | isHardExpert attrs -> do
      let
        mHex =
          find (`cardMatch` CardWithTrait Trait.Hex) (scenarioDiscard attrs)
      for_ mHex $ \hex -> do
        pushAll
          [ RemoveFromEncounterDiscard hex
          , ShuffleCardsIntoDeck
            (Deck.ScenarioDeckByKey ExplorationDeck)
            [EncounterCard hex]
          ]
      pure s
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    _ -> ShatteredAeons <$> runMessage msg attrs
