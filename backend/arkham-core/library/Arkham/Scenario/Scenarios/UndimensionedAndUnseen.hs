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
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Attrs ( Field (..) )
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Helpers
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( ChosenRandomLocation, RevealLocation )
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
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

undimensionedAndUnseenIntro :: Message
undimensionedAndUnseenIntro = FlavorText
  (Just "Scenario V: Undimensioned and Unseen")
  [ "Your search of the village of Dunwich has\
    \ uncovered a number of documents, journal entries,\
    \ and esoteric theories. Reading through these\
    \ materials leaves you exhausted and emotionally\
    \ drained. Most of the content was written by\
    \ a single source—a man named Seth Bishop.\
    \ When you ask around town, you learn that Seth\
    \ is a citizen of Dunwich. Along with several others, Seth had witnessed\
    \ firsthand the devastation wrought by the events of “the Dunwich\
    \ horror,” as Armitage had dubbed the incident. Curiously, since that time,\
    \ very few people had seen Seth around town, and those who did claimed\
    \ his eyes had been bloodshot and his face sweaty and pale."
  , "You don’t doubt that somebody who has seen what Seth has seen would\
    \ appear nervous or paranoid. But the more you read of his frantic and\
    \ unhinged writings, the more you believe he is involved in recent events.\
    \ His writings speak of having “gathered the remains” and of using\
    \ arcane methods to “imbue the fathers’ essence” into other creatures, and\
    \ eventually, into other people. The explanations and diagrams that follow\
    \ are unfathomably complex and defy understanding."
  , "Before you are able to find Seth and confront him, several men and\
    \ women from the village approach you in a panic. “It’s back!” one of them\
    \ wails. You recognize him as Curtis Whateley, of the undecayed branch.\
    \ “Whatever it was that killed them Fryes, it’s back! Up and smashed the\
    \ Bishops’ home like it were made o’ paper!” Curtis and the other townsfolk\
    \ are clamoring amongst themselves, raising their voices in a panic."
  ]

undimensionedAndUnseenPart1 :: Message
undimensionedAndUnseenPart1 = FlavorText
  Nothing
  [ "You aim to calm the townsfolk so they can explain to you what\
    \ is going on. They inform you that there was a rumbling to the north, and\
    \ when they went to investigate they found the Bishops’ farmhouse had\
    \ been torn to shreds. A trail of heavy tracks led into nearby Cold Spring\
    \ Glen. “You know what to do, right? You Arkham folk stopped that thing\
    \ last time,” one of the townsfolk says. Curtis shakes his head and bites at\
    \ his lip."
  , "“We couldn’t even see that hellish thing until the old professor sprayed\
    \ that there powder on it,” He says. “To this day, I wish I hadn’t seen it at\
    \ all…” Something must be done to stop the monster’s rampage. But, if the\
    \ documents you found are true, there may be more than one such creature\
    \ on the loose…."
  ]

undimensionedAndUnseenPart2 :: Message
undimensionedAndUnseenPart2 = FlavorText
  Nothing
  [ "You warn the townsfolk that they are in grave danger, and urge\
    \ them to flee Dunwich while they can. Several of them immediately heed\
    \ your advice, remembering the terrible monstrosity that had previously\
    \ endangered the town. Curtis drops to his knees in despair, sweating\
    \ feverishly. “It’s that thing again, ain’t it? It’s come back fer us,” Curtis\
    \ stutters. “I hope you’ve got some of that powder the old professor had last\
    \ time. We couldn’t even see the damned thing until he sprayed it. To this\
    \ day, I wish I hadn’t seen it at all…” Something must be done to stop the\
    \ monster’s rampage. But, if the documents you found are true, there may\
    \ be more than one such creature on the loose…."
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
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ pushAll
        [ story investigatorIds undimensionedAndUnseenIntro
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
      investigatorIds <- getInvestigatorIds
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

      let
        dunwichVillageId = toLocationId dunwichVillage
        coldSpringGlenId = toLocationId coldSpringGlen
        blastedHeathId = toLocationId blastedHeath

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

      let
        locations =
          [ dunwichVillage
          , coldSpringGlen
          , tenAcreMeadow
          , blastedHeath
          , whateleyRuins
          , devilsHopYard
          ]

      pushAll
        $ [ story
            investigatorIds
            (if n == 1
              then undimensionedAndUnseenPart1
              else undimensionedAndUnseenPart2
            )
          , Record
            (if n == 1 then YouCalmedTheTownsfolk else YouWarnedTheTownsfolk)
          , SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          ]
        <> [ PlaceLocation location | location <- locations ]
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
                (push $ EnemyAttack iid eid DamageAny RegularAttack)
            _ -> pure s
        _ -> pure s
    RequestedPlayerCard iid source mcard | isSource attrs source ->
      case mcard of
        Nothing -> pure s
        Just card -> s <$ push (ShuffleCardsIntoDeck iid [card])
    ScenarioResolution NoResolution ->
      s <$ pushAll [ScenarioResolution $ Resolution 1]
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      broodEscapedIntoTheWild <-
        (+ count ((== "02255") . toCardCode) (scenarioSetAsideCards attrs))
        . length
        <$> getBroodOfYogSothoth
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 1")
                 [ "You did all you could to stop the rampaging\
                         \ monsters, but there were more of them than you realized and\
                         \ you weren’t able to slay them all. Exhausted and terrified, you\
                         \ retreat to Zebulon’s home and hope to survive the night."
                 ]
               ]
           ]
         , RecordCount BroodEscapedIntoTheWild broodEscapedIntoTheWild
         ]
        <> [ RemoveCampaignCardFromDeck iid "02219" | iid <- investigatorIds ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 2")
                 [ "After slaying what seems to be the last of\
                     \ the rampaging monsters you retreat to Zebulon’s home,\
                     \ exhausted and rattled by your experience"
                 ]
               ]
           ]
         , Record NoBroodEscapedIntoTheWild
         ]
        <> [ RemoveCampaignCardFromDeck iid "02219" | iid <- investigatorIds ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    _ -> UndimensionedAndUnseen <$> runMessage msg attrs
