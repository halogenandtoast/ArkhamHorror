module Arkham.Types.Scenario.Scenarios.UndimensionedAndUnseen
  ( UndimensionedAndUnseen(..)
  , undimensionedAndUnseen
  ) where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
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

newtype UndimensionedAndUnseenMetadata = UndimensionedAndUnseenMetadata { setAsideSpawnOfYogSothoth :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UndimensionedAndUnseen = UndimensionedAndUnseen (ScenarioAttrs `With` UndimensionedAndUnseenMetadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undimensionedAndUnseen :: Difficulty -> UndimensionedAndUnseen
undimensionedAndUnseen difficulty =
  UndimensionedAndUnseen . (`with` UndimensionedAndUnseenMetadata 0) $ base
    { scenarioLocationLayout = Just
      [ ". blastedHeath devilsHopYard"
      , ". blastedHeath devilsHopYard"
      , "dunwichVillage tenAcreMeadow ."
      , "dunwichVillage tenAcreMeadow whateleyRuins"
      , ". coldSpringGlen whateleyRuins"
      , ". coldSpringGlen ."
      ]
    }
 where
  base = baseAttrs
    "02236"
    "Undimensioned and Unseen"
    ["02237", "02238", "02239"]
    ["02240", "02241"]
    difficulty

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

instance HasRecord UndimensionedAndUnseen where
  hasRecord _ _ = False
  hasRecordSet SacrificedToYogSothoth _ = ["02040"]
  hasRecordSet _ _ = []

instance
  ( HasSet StoryEnemyId env CardCode
  , HasTokenValue env InvestigatorId
  )
  => HasTokenValue env UndimensionedAndUnseen where
  getTokenValue (UndimensionedAndUnseen (attrs `With` _)) iid = \case
    Skull -> do
      broodCount <- length <$> getSetList @StoryEnemyId (CardCode "02255")
      pure $ toTokenValue attrs Skull broodCount (2 * broodCount)
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet ZeroModifier
    ElderThing -> pure $ toTokenValue attrs ElderThing 3 5
    otherFace -> getTokenValue attrs iid otherFace

instance
  ( HasId LeadInvestigatorId env ()
  , HasId CardCode env EnemyId
  , HasSource ForSkillTest env
  , HasSet InvestigatorId env ()
  , HasList DeckCard env InvestigatorId
  , HasRecord env
  , ScenarioAttrsRunner env
  )
  => RunMessage env UndimensionedAndUnseen where
  runMessage msg s@(UndimensionedAndUnseen (attrs `With` metadata)) =
    case msg of
      SetTokensForScenario -> do
        standalone <- getIsStandalone
        s <$ if standalone
          then unshiftMessage (SetTokens standaloneTokens)
          else pure ()
      Setup -> do
        investigatorIds <- getInvestigatorIds
        leadInvestigatorId <- getLeadInvestigatorId
        s <$ unshiftMessages
          [ story investigatorIds undimensionedAndUnseenIntro
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
          [ EncounterSet.UndimensionedAndUnseen
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
              (if n == 1
                then undimensionedAndUnseenPart1
                else undimensionedAndUnseenPart2
              )
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

        UndimensionedAndUnseen
          . (`with` UndimensionedAndUnseenMetadata setAsideCount)
          <$> runMessage msg (attrs & locationsL .~ locations')
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
      _ -> UndimensionedAndUnseen . (`with` metadata) <$> runMessage msg attrs
