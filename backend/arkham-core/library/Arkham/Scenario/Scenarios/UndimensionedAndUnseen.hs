module Arkham.Scenario.Scenarios.UndimensionedAndUnseen
  ( UndimensionedAndUnseen(..)
  , undimensionedAndUnseen
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
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Game.Helpers
import Arkham.Helpers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( ChosenRandomLocation, RevealLocation )
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Scenarios.UndimensionedAndUnseen.Story
import Arkham.SkillTest
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait hiding ( Cultist )
import Arkham.Window ( defaultWindows )

newtype UndimensionedAndUnseen = UndimensionedAndUnseen ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undimensionedAndUnseen :: Difficulty -> UndimensionedAndUnseen
undimensionedAndUnseen difficulty = scenario
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

standaloneTokens :: [TokenFace]
standaloneTokens =
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
standaloneCampaignLog = mkCampaignLog
  { campaignLogRecordedSets = mapFromList
    [(SacrificedToYogSothoth, [Recorded "02040"])]
  }

instance HasTokenValue UndimensionedAndUnseen where
  getTokenValue iid tokenFace (UndimensionedAndUnseen attrs) =
    case tokenFace of
      Skull -> do
        broodCount <- length <$> getBroodOfYogSothoth
        pure $ toTokenValue attrs Skull broodCount (2 * broodCount)
      Cultist -> pure $ TokenValue Cultist NoModifier
      Tablet -> pure $ TokenValue Tablet ZeroModifier
      ElderThing -> pure $ toTokenValue attrs ElderThing 3 5
      otherFace -> getTokenValue iid otherFace attrs

instance RunMessage UndimensionedAndUnseen where
  runMessage msg s@(UndimensionedAndUnseen attrs) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      s <$ if standalone then push (SetTokens standaloneTokens) else pure ()
    StandaloneSetup ->
      pure
        . UndimensionedAndUnseen
        $ attrs
        & standaloneCampaignLogL
        .~ standaloneCampaignLog
    Setup -> do
      investigatorIds <- allInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ pushAll
        [ story investigatorIds intro
        , chooseOne
          leadInvestigatorId
          [ Label
            "You try to calm down the townsfolk in order to learn more."
            [SetupStep (toTarget attrs) 1]
          , Label
            "You try to warn the townsfolk and convince them to evacuate."
            [SetupStep (toTarget attrs) 2]
          ]
        ]
    SetupStep (isTarget attrs -> True) n -> do
      standalone <- getIsStandalone
      investigatorIds <- allInvestigatorIds
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.broodOfYogSothoth, Assets.esotericFormula]
        [ EncounterSet.UndimensionedAndUnseen
        , EncounterSet.Whippoorwills
        , EncounterSet.BeastThralls
        , EncounterSet.Dunwich
        , EncounterSet.StrikingFear
        ]

      dunwichVillage <- genCard =<< sample
        (Locations.dunwichVillage_242 :| [Locations.dunwichVillage_243])
      coldSpringGlen <- genCard =<< sample
        (Locations.coldSpringGlen_244 :| [Locations.coldSpringGlen_245])
      tenAcreMeadow <- genCard =<< sample
        (Locations.tenAcreMeadow_246 :| [Locations.tenAcreMeadow_247])
      blastedHeath <-
        genCard =<< sample
          (Locations.blastedHeath_248 :| [Locations.blastedHeath_249])
      whateleyRuins <- genCard =<< sample
        (Locations.whateleyRuins_250 :| [Locations.whateleyRuins_251])
      devilsHopYard <- genCard =<< sample
        (Locations.devilsHopYard_252 :| [Locations.devilsHopYard_253])

      (dunwichVillageId, placeDunwichVillage) <- placeLocation dunwichVillage
      (coldSpringGlenId, placeColdSpringGlen) <- placeLocation coldSpringGlen
      (blastedHeathId, placeBlastedHeath) <- placeLocation blastedHeath
      placeTenAcreMeadow <- placeLocation_ tenAcreMeadow
      placeWhateleyRuins <- placeLocation_ whateleyRuins
      placeDevilsHopYard <- placeLocation_ devilsHopYard

      sacrificedToYogSothoth <- if standalone
        then pure 3
        else length <$> getRecordSet SacrificedToYogSothoth

      investigatorsWithPowderOfIbnGhazi <- catMaybes <$> for
        investigatorIds
        (\iid -> do
          powderOfIbnGhazi <-
            find ((== "02219") . toCardCode)
              <$> fieldMap InvestigatorDeck unDeck iid
          pure $ (iid, ) <$> powderOfIbnGhazi
        )

      (msgs, setAsideCount) <- case sacrificedToYogSothoth of
        2 -> do
          broodOfYogSothoth <- EncounterCard
            <$> genEncounterCard Enemies.broodOfYogSothoth
          pure ([CreateEnemyAt broodOfYogSothoth coldSpringGlenId Nothing], 3)
        3 -> do
          broodOfYogSothoth <- EncounterCard
            <$> genEncounterCard Enemies.broodOfYogSothoth
          pure ([CreateEnemyAt broodOfYogSothoth coldSpringGlenId Nothing], 2)
        x -> if x <= 2
          then do
            broodOfYogSothoth1 <- EncounterCard
              <$> genEncounterCard Enemies.broodOfYogSothoth
            broodOfYogSothoth2 <- EncounterCard
              <$> genEncounterCard Enemies.broodOfYogSothoth
            pure
              ( [ CreateEnemyAt broodOfYogSothoth1 coldSpringGlenId Nothing
                , CreateEnemyAt broodOfYogSothoth2 blastedHeathId Nothing
                ]
              , 3
              )
          else pure ([], 2)

      setAsideBroodOfYogSothoth <- replicateM
        setAsideCount
        (genCard Enemies.broodOfYogSothoth)

      setAsideCards <- replicateM 4 (genCard Assets.esotericFormula)

      pushAll
        $ [ story investigatorIds (if n == 1 then introPart1 else introPart2)
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
               iid
               [ Label
                 "Play Powder of Ibn-Ghazi"
                 [ PutCardIntoPlay
                     iid
                     (PlayerCard card)
                     Nothing
                     (defaultWindows iid)
                 ]
               , Label "Do no play Powder of Ibn-Ghazi" []
               ]
           | (iid, card) <- investigatorsWithPowderOfIbnGhazi
           ]
        <> [ SearchCollectionForRandom
               iid
               (toSource attrs)
               (CardWithType PlayerTreacheryType
               <> CardWithOneOf (map CardWithTrait [Madness, Injury, Pact])
               )
           | not standalone
           , iid <- investigatorIds
           ]
        <> msgs

      UndimensionedAndUnseen <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideBroodOfYogSothoth <> setAsideCards)
        & (actStackL
          . at 1
          ?~ [Acts.saracenicScript, Acts.theyMustBeDestroyed]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.rampagingCreatures
             , Agendas.bidingItsTime
             , Agendas.horrorsUnleashed
             ]
          )
        )
    ResolveToken drawnToken Tablet _ -> s <$ push
      (CreateEffect
        "02236"
        Nothing
        (TokenSource drawnToken)
        (TokenTarget drawnToken)
      )
    ResolveToken _ ElderThing iid -> do
      msource <- getSkillTestSource
      case msource of
        Just (SkillTestSource _ _ _ (Just action)) -> do
          mtarget <- getSkillTestTarget
          case mtarget of
            Just (EnemyTarget eid) -> do
              enemyCardCode <- field EnemyCardCode eid
              s <$ when
                (enemyCardCode
                == "02255"
                && (action `elem` [Action.Evade, Action.Fight])
                )
                (push $ EnemyAttack $ enemyAttack eid iid)
            _ -> pure s
        _ -> pure s
    RequestedPlayerCard iid source mcard | isSource attrs source -> do
      for_ mcard $ \card -> push
        $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [PlayerCard card]
      pure s
    ScenarioResolution NoResolution ->
      s <$ pushAll [ScenarioResolution $ Resolution 1]
    ScenarioResolution (Resolution 1) -> do
      investigatorIds <- allInvestigatorIds
      xp <- getXp
      broodEscapedIntoTheWild <-
        (+ count ((== "02255") . toCardCode) (scenarioSetAsideCards attrs))
        . length
        <$> getBroodOfYogSothoth
      pushAll
        $ [ story investigatorIds resolution1
          , RecordCount BroodEscapedIntoTheWild broodEscapedIntoTheWild
          ]
        <> [RemoveCampaignCard Assets.powderOfIbnGhazi]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 2) -> do
      investigatorIds <- allInvestigatorIds
      xp <- getXp
      pushAll
        $ [story investigatorIds resolution2, Record NoBroodEscapedIntoTheWild]
        <> [RemoveCampaignCard Assets.powderOfIbnGhazi]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    _ -> UndimensionedAndUnseen <$> runMessage msg attrs
