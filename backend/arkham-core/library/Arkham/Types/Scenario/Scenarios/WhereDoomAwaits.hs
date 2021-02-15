module Arkham.Types.Scenario.Scenarios.WhereDoomAwaits
  ( WhereDoomAwaits(..)
  , whereDoomAwaits
  )
where

import Arkham.Prelude

import Arkham.Types.Resolution
import Arkham.EncounterCard
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.LocationId
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import qualified Arkham.Types.Action as Action
import Arkham.Types.Game.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Token
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait hiding (Cultist)

newtype WhereDoomAwaits = WhereDoomAwaits ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereDoomAwaits :: Difficulty -> WhereDoomAwaits
whereDoomAwaits difficulty = WhereDoomAwaits $ base
  { scenarioLocationLayout = Just
    [ "divergingPath1 divergingPath2 divergingPath3"
    , "baseOfTheHill ascendingPath sentinelPeak"
    , "alteredPath1 alteredPath2 alteredPath3"
    ]
  }
 where
  base = baseAttrs
    "02274"
    "Where Doom Awaits"
    ["02275", "02276"]
    ["02277", "02278", "02279", "02280", "02281"]
    difficulty

whereDoomAwaitsIntro :: Message
whereDoomAwaitsIntro = FlavorText
  (Just "Scenario VI: Where Doom Awaits")
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

whereDoomAwaitsPart1 :: Message
whereDoomAwaitsPart1 = FlavorText
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

whereDoomAwaitsPart2 :: Message
whereDoomAwaitsPart2 = FlavorText
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

standaloneTokens :: [Token]
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

instance HasRecord WhereDoomAwaits where
  hasRecord _ _ = False
  hasRecordSet SacrificedToYogSothoth _ = ["02040"]
  hasRecordSet _ _ = []
  hasRecordCount _ _ = 0

instance
  ( HasSet StoryEnemyId env CardCode
  , HasTokenValue env InvestigatorId
  )
  => HasTokenValue env WhereDoomAwaits where
  getTokenValue (WhereDoomAwaits attrs) iid = \case
    Skull -> do
      broodCount <- length <$> getSetList @StoryEnemyId (CardCode "02255")
      pure $ toTokenValue attrs Skull broodCount (2 * broodCount)
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet ZeroModifier
    ElderThing -> pure $ toTokenValue attrs ElderThing 3 5
    otherFace -> getTokenValue attrs iid otherFace

instance
  ( HasId CardCode env EnemyId
  , HasSource ForSkillTest env
  , HasCount XPCount env ()
  , HasSet InvestigatorId env ()
  , HasSet LocationId env [Trait]
  , HasSet Trait env LocationId
  , HasSet StoryEnemyId env CardCode
  , HasList DeckCard env InvestigatorId
  , HasRecord env
  , ScenarioAttrsRunner env
  )
  => RunMessage env WhereDoomAwaits where
  runMessage msg s@(WhereDoomAwaits attrs) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      s <$ if standalone
        then unshiftMessage (SetTokens standaloneTokens)
        else pure ()
    Setup -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessages
        [ story investigatorIds whereDoomAwaitsIntro
        , chooseOne
          leadInvestigatorId
          [ Label
            "You try to calm down the townsfolk in order to learn more."
            [SetupStep 1]
          , Label
            "You try to warn the townsfolk and convince them to evacuate."
            [SetupStep 2]
          ]
        ]
    SetupStep n -> do
      standalone <- getIsStandalone
      investigatorIds <- getInvestigatorIds
      encounterDeck <- buildEncounterDeckExcluding
        ["02255"]
        [ EncounterSet.WhereDoomAwaits
        , EncounterSet.Whippoorwills
        , EncounterSet.BeastThralls
        , EncounterSet.Dunwich
        , EncounterSet.StrikingFear
        ]

      dunwichVillage <- sample $ "02242" :| ["02243"]
      coldSpringGlen <- sample $ "02244" :| ["02245"]
      tenAcreMeadow <- sample $ "02246" :| ["02247"]
      blastedHeath <- sample $ "02248" :| ["02249"]
      whateleyRuins <- sample $ "02250" :| ["02251"]
      devilsHopYard <- sample $ "00252" :| ["02253"]

      sacrificedToYogSothoth <- if standalone
        then pure 3
        else length <$> asks (hasRecordSet SacrificedToYogSothoth)

      investigatorsWithPowderOfIbnGhazi <- catMaybes <$> for
        investigatorIds
        (\iid -> do
          powderOfIbnGhazi <-
            find ((== "02219") . getCardCode) . map unDeckCard <$> getList iid
          pure $ (iid, ) <$> powderOfIbnGhazi
        )

      (msgs, setAsideCount) <- case sacrificedToYogSothoth of
        2 -> do
          broodOfYogSothoth <- EncounterCard <$> genEncounterCard "02255"
          pure ([CreateEnemyAt broodOfYogSothoth coldSpringGlen], 3)
        3 -> do
          broodOfYogSothoth <- EncounterCard <$> genEncounterCard "02255"
          pure ([CreateEnemyAt broodOfYogSothoth coldSpringGlen], 2)
        x -> if x <= 2
          then do
            broodOfYogSothoth1 <- EncounterCard <$> genEncounterCard "02255"
            broodOfYogSothoth2 <- EncounterCard <$> genEncounterCard "02255"
            pure
              ( [ CreateEnemyAt broodOfYogSothoth1 coldSpringGlen
                , CreateEnemyAt broodOfYogSothoth2 blastedHeath
                ]
              , 3
              )
          else pure ([], 2)

      setAsideBroodOfYogSothoth <- replicateM
        setAsideCount
        (EncounterCard <$> genEncounterCard "02255")

      let
        locations =
          [ dunwichVillage
          , coldSpringGlen
          , tenAcreMeadow
          , blastedHeath
          , whateleyRuins
          , devilsHopYard
          ]
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocation))
          locations

      unshiftMessages
        $ [ story
            investigatorIds
            (if n == 1 then whereDoomAwaitsPart1 else whereDoomAwaitsPart2)
          , Record
            (if n == 1 then YouCalmedTheTownsfolk else YouWarnedTheTownsfolk)
          , SetEncounterDeck encounterDeck
          , AddAgenda "02237"
          , AddAct "02240"
          ]
        <> map PlaceLocation locations
        <> [RevealLocation Nothing dunwichVillage, MoveAllTo dunwichVillage]
        <> [ chooseOne
               iid
               [ Label
                 "Play Powder of Ibn-Ghazi"
                 [PutCardIntoPlay iid (PlayerCard card) Nothing]
               , Label "Do no play Powder of Ibn-Ghazi" []
               ]
           | (iid, card) <- investigatorsWithPowderOfIbnGhazi
           ]
        <> [ SearchCollectionForRandom
               iid
               (toSource attrs)
               (PlayerTreacheryType, setFromList [Madness, Injury, Pact])
           | not standalone
           , iid <- investigatorIds
           ]
        <> msgs

      WhereDoomAwaits <$> runMessage
        msg
        (attrs
        & locationsL
        .~ locations'
        & setAsideCardsL
        .~ setAsideBroodOfYogSothoth
        )
    ResolveToken drawnToken Tablet _ -> s <$ unshiftMessage
      (CreateEffect
        "02236"
        Nothing
        (DrawnTokenSource drawnToken)
        (DrawnTokenTarget drawnToken)
      )
    ResolveToken _ ElderThing iid -> do
      msource <- asks $ getSource ForSkillTest
      case msource of
        Just (SkillTestSource _ _ _ (EnemyTarget eid) (Just action)) -> do
          enemyCardCode <- getId @CardCode eid
          s <$ when
            (enemyCardCode
            == "02255"
            && (action `elem` [Action.Evade, Action.Fight])
            )
            (unshiftMessage $ EnemyAttack iid eid)
        _ -> pure s
    RequestedPlayerCard iid source mcard | isSource attrs source ->
      case mcard of
        Nothing -> pure s
        Just card -> s <$ unshiftMessage (ShuffleCardsIntoDeck iid [card])
    ScenarioResolution NoResolution ->
      s <$ unshiftMessages [ScenarioResolution $ Resolution 1]
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      broodEscapedIntoTheWild <-
        (+ count ((== "02255") . getCardCode) (scenarioSetAsideCards attrs))
        . length
        <$> getSetList @StoryEnemyId (CardCode "02255")
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
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
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "After slaying what seems to be the last of\
                     \ the rampaging monsters you retreat to Zebulon’s home,\
                     \ exhausted and rattled by your experience"
                 ]
               ]
           ]
         , Record NoBroodEscapedIntoTheWild
         ]
        <> [ RemoveCampaignCardFromDeck iid "02219" | iid <- investigatorIds ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    UseScenarioSpecificAbility _ Nothing 1 ->
      s <$ unshiftMessage (ChooseRandomLocation (toTarget attrs) mempty)
    UseScenarioSpecificAbility _ (Just (LocationTarget lid)) 1 ->
      case scenarioSetAsideCards attrs of
        [] -> error "should not call when empty"
        (x : xs) -> do
          unshiftMessage (CreateEnemyAt x lid)
          pure . WhereDoomAwaits $ attrs & setAsideCardsL .~ xs
    ChosenRandomLocation target randomLocationId | isTarget attrs target -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessage
        (UseScenarioSpecificAbility
          leadInvestigatorId
          (Just (LocationTarget randomLocationId))
          1
        )
    PlacedLocation lid -> do
      traits <- getSet lid
      when (Woods `member` traits) $ do
        woodsCount <- length <$> getSetList @LocationId [Woods]
        unshiftMessage
          (SetLocationLabel lid $ "divergingPath" <> tshow woodsCount)
      when (Altered `member` traits) $ do
        alteredCount <- length <$> getSetList @LocationId [Woods]
        unshiftMessage
          (SetLocationLabel lid $ "alteredPathPath" <> tshow alteredCount)
      pure s
    _ -> WhereDoomAwaits <$> runMessage msg attrs
