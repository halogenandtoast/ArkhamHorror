module Arkham.Scenario.Scenarios.UndimensionedAndUnseen (
  UndimensionedAndUnseen (..),
  undimensionedAndUnseen,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Cost
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Game.Helpers
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (ChosenRandomLocation, RevealLocation)
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Scenarios.UndimensionedAndUnseen.Story
import Arkham.SkillTest
import Arkham.Trait hiding (Cultist)
import Arkham.Window (defaultWindows)

newtype UndimensionedAndUnseen = UndimensionedAndUnseen ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undimensionedAndUnseen :: Difficulty -> UndimensionedAndUnseen
undimensionedAndUnseen difficulty =
  scenario
    UndimensionedAndUnseen
    "02236"
    "Undimensioned and Unseen"
    difficulty
    [ ". blastedHeath devilsHopYard"
    , ". blastedHeath devilsHopYard"
    , "dunwichVillage tenAcreMeadow ."
    , "dunwichVillage tenAcreMeadow whateleyRuins"
    , ". coldSpringGlen whateleyRuins"
    , ". coldSpringGlen ."
    ]

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
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecordedSets =
        mapFromList
          [(SacrificedToYogSothoth, [recorded @CardCode "02040"])]
    }

instance HasChaosTokenValue UndimensionedAndUnseen where
  getChaosTokenValue iid chaosTokenFace (UndimensionedAndUnseen attrs) =
    case chaosTokenFace of
      Skull -> do
        broodCount <- length <$> getBroodOfYogSothoth
        pure $ toChaosTokenValue attrs Skull broodCount (2 * broodCount)
      Cultist -> pure $ ChaosTokenValue Cultist NoModifier
      Tablet -> pure $ ChaosTokenValue Tablet ZeroModifier
      ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
      otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage UndimensionedAndUnseen where
  runMessage msg s@(UndimensionedAndUnseen attrs) = case msg of
    SetChaosTokensForScenario -> do
      whenStandalone $ push (SetChaosTokens standaloneChaosTokens)
      pure s
    StandaloneSetup ->
      pure
        . UndimensionedAndUnseen
        $ attrs
        & standaloneCampaignLogL
        .~ standaloneCampaignLog
    Setup -> do
      players <- allPlayers
      lead <- getLeadPlayer
      pushAll
        [ story players intro
        , chooseOne
            lead
            [ Label
                "You try to calm down the townsfolk in order to learn more."
                [SetupStep (toTarget attrs) 1]
            , Label
                "You try to warn the townsfolk and convince them to evacuate."
                [SetupStep (toTarget attrs) 2]
            ]
        ]
      pure s
    SetupStep (isTarget attrs -> True) n -> do
      standalone <- getIsStandalone
      investigatorIds <- allInvestigatorIds
      players <- allPlayers
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.broodOfYogSothoth, Assets.esotericFormula]
          [ EncounterSet.UndimensionedAndUnseen
          , EncounterSet.Whippoorwills
          , EncounterSet.BeastThralls
          , EncounterSet.Dunwich
          , EncounterSet.StrikingFear
          ]

      dunwichVillage <-
        genCard
          =<< sample
            (Locations.dunwichVillage_242 :| [Locations.dunwichVillage_243])
      coldSpringGlen <-
        genCard
          =<< sample
            (Locations.coldSpringGlen_244 :| [Locations.coldSpringGlen_245])
      tenAcreMeadow <-
        genCard
          =<< sample
            (Locations.tenAcreMeadow_246 :| [Locations.tenAcreMeadow_247])
      blastedHeath <-
        genCard
          =<< sample
            (Locations.blastedHeath_248 :| [Locations.blastedHeath_249])
      whateleyRuins <-
        genCard
          =<< sample
            (Locations.whateleyRuins_250 :| [Locations.whateleyRuins_251])
      devilsHopYard <-
        genCard
          =<< sample
            (Locations.devilsHopYard_252 :| [Locations.devilsHopYard_253])

      (dunwichVillageId, placeDunwichVillage) <- placeLocation dunwichVillage
      (coldSpringGlenId, placeColdSpringGlen) <- placeLocation coldSpringGlen
      (blastedHeathId, placeBlastedHeath) <- placeLocation blastedHeath
      placeTenAcreMeadow <- placeLocation_ tenAcreMeadow
      placeWhateleyRuins <- placeLocation_ whateleyRuins
      placeDevilsHopYard <- placeLocation_ devilsHopYard

      sacrificedToYogSothoth <-
        if standalone
          then pure 3
          else length <$> getRecordSet SacrificedToYogSothoth

      investigatorsWithPowderOfIbnGhazi <-
        catMaybes <$> for investigatorIds \iid -> do
          powderOfIbnGhazi <- find ((== "02219") . toCardCode) <$> fieldMap InvestigatorDeck unDeck iid
          player <- getPlayer iid
          pure $ (iid,player,) <$> powderOfIbnGhazi

      broodOfYogSothoth <- genCard Enemies.broodOfYogSothoth
      createBroodOfYogSothoth <-
        createEnemyAt_
          broodOfYogSothoth
          coldSpringGlenId
          Nothing
      (msgs, setAsideCount) <- case sacrificedToYogSothoth of
        2 -> pure ([createBroodOfYogSothoth], 3)
        3 -> pure ([createBroodOfYogSothoth], 2)
        x ->
          if x <= 2
            then do
              broodOfYogSothoth2 <- genCard Enemies.broodOfYogSothoth
              createBroodOfYogSothoth2 <-
                createEnemyAt_
                  broodOfYogSothoth2
                  blastedHeathId
                  Nothing
              pure ([createBroodOfYogSothoth, createBroodOfYogSothoth2], 3)
            else pure ([], 2)

      setAsideBroodOfYogSothoth <-
        replicateM
          setAsideCount
          (genCard Enemies.broodOfYogSothoth)

      setAsideCards <- replicateM 4 (genCard Assets.esotericFormula)

      pushAll
        $ [ story players (if n == 1 then introPart1 else introPart2)
          , Record
              (if n == 1 then YouCalmedTheTownsfolk else YouWarnedTheTownsfolk)
          , SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeDunwichVillage
          , placeColdSpringGlen
          , placeTenAcreMeadow
          , placeBlastedHeath
          , placeWhateleyRuins
          , placeDevilsHopYard
          ]
        <> [ RevealLocation Nothing dunwichVillageId
           , MoveAllTo (toSource attrs) dunwichVillageId
           ]
        <> [ chooseOne
            player
            [ Label
                "Play Powder of Ibn-Ghazi"
                [ PutCardIntoPlay
                    iid
                    (PlayerCard card)
                    Nothing
                    NoPayment
                    (defaultWindows iid)
                ]
            , Label "Do no play Powder of Ibn-Ghazi" []
            ]
           | (iid, player, card) <- investigatorsWithPowderOfIbnGhazi
           ]
        <> [ SearchCollectionForRandom
            iid
            (toSource attrs)
            ( CardWithType PlayerTreacheryType
                <> CardWithOneOf (map CardWithTrait [Madness, Injury, Pact])
            )
           | not standalone
           , iid <- investigatorIds
           ]
        <> msgs

      agendas <-
        genCards
          [ Agendas.rampagingCreatures
          , Agendas.bidingItsTime
          , Agendas.horrorsUnleashed
          ]
      acts <- genCards [Acts.saracenicScript, Acts.theyMustBeDestroyed]

      UndimensionedAndUnseen
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideBroodOfYogSothoth <> setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken drawnToken Tablet _ -> do
      push
        $ CreateEffect
          "02236"
          Nothing
          (ChaosTokenSource drawnToken)
          (ChaosTokenTarget drawnToken)
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      mAction <- getSkillTestAction
      case mAction of
        Just action | action `elem` [Action.Evade, Action.Fight] -> do
          mTarget <- getSkillTestTarget
          case mTarget of
            Just (EnemyTarget eid) -> do
              enemyCardCode <- field EnemyCardCode eid
              pushWhen (enemyCardCode == "02255")
                $ EnemyAttack
                $ enemyAttack eid attrs iid
            _ -> pure ()
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist ->
          push
            $ InvestigatorAssignDamage
              iid
              (ChaosTokenEffectSource Cultist)
              DamageAny
              (if isHardExpert attrs then 1 else 0)
              1
        _ -> pure ()
      pure s
    RequestedPlayerCard iid source mcard _ | isSource attrs source -> do
      for_ mcard $ \card ->
        push
          $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [PlayerCard card]
      pure s
    ScenarioResolution NoResolution ->
      s <$ pushAll [ScenarioResolution $ Resolution 1]
    ScenarioResolution (Resolution 1) -> do
      players <- allPlayers
      xp <- getXp
      broodEscapedIntoTheWild <-
        (+ count ((== "02255") . toCardCode) (scenarioSetAsideCards attrs))
          . length
          <$> getBroodOfYogSothoth
      pushAll
        $ [ story players resolution1
          , RecordCount BroodEscapedIntoTheWild broodEscapedIntoTheWild
          ]
        <> [RemoveCampaignCard Assets.powderOfIbnGhazi]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 2) -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [story players resolution2, Record NoBroodEscapedIntoTheWild]
        <> [RemoveCampaignCard Assets.powderOfIbnGhazi]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    _ -> UndimensionedAndUnseen <$> runMessage msg attrs
