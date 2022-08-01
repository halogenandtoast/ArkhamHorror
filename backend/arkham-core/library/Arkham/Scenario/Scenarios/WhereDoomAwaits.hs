module Arkham.Scenario.Scenarios.WhereDoomAwaits
  ( WhereDoomAwaits(..)
  , whereDoomAwaits
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types ( Field (..) )
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.Cost
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait hiding ( Cultist, Expert )
import Data.Maybe ( fromJust )

newtype WhereDoomAwaits = WhereDoomAwaits ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereDoomAwaits :: Difficulty -> WhereDoomAwaits
whereDoomAwaits difficulty = scenario
  WhereDoomAwaits
  "02274"
  "Where Doom Awaits"
  difficulty
  [ "divergingPath1 divergingPath2 divergingPath3"
  , "baseOfTheHill ascendingPath sentinelPeak"
  , "alteredPath1 alteredPath2 alteredPath3"
  ]

whereDoomAwaitsIntro :: Message
whereDoomAwaitsIntro = FlavorText
  (Just "Scenario VI: Where Doom Awaits")
  [ "You awaken to the sound of screeching. Fearing\
    \ the worst, you grab your equipment and head\
    \ out into the streets of Dunwich. As soon as you\
    \ step outside, you sense a foulness in the cold night\
    \ air: an awful, pungent smell that can scarcely\
    \ be described and a heaviness to the atmosphere\
    \ that makes it difficult to breathe. The citizens of\
    \ Dunwich have sealed their doors, and the town feels quiet and lonesome.\
    \ In the distance, a faint glow emanates from a hilltop above the village.\
    \ You know of this hill from both your interactions with Zebulon and\
    \ Armitage’s records. It is called Sentinel Hill. The tales speak of satanic\
    \ rites being performed there—rites in which great ritual-pyres light up\
    \ the night sky while the ground rumbles furiously below."
  , "Flocks of whippoorwills perch on the rooftops of the village around you,\
    \ watching ominously as you climb inside Zebulon’s old and beat-up truck.\
    \ As you drive towards Sentinel Hill, more screeching fills the sky with an\
    \ awful pitch that is painful to your ears. Everything you have read about\
    \ and experienced in Dunwich has led to this. If the foul ritual Seth seeks\
    \ to perform has anything to do with what Armitage and his colleagues\
    \ prevented several months back, it involves the favor of an ancient\
    \ creature—Yog‑Sothoth. Failing to stop this ritual may spell doom...not\
    \ only Dunwich, but for the entire world."
  ]

whereDoomAwaitsPart1 :: Message
whereDoomAwaitsPart1 = FlavorText
  Nothing
  [ "The path leading up Sentinel Hill is narrow and too torn up for\
    \ Zebulon’s truck, so you park at the base of the hill and prepare to make\
    \ the rest of the trip on foot. Just then, you notice that you are not alone.\
    \ Several men and women emerge from the woods behind you, brandishing\
    \ firearms and lining you up in their sights. You raise your hands and brace\
    \ for the worst. “Wait,” one of them says, raising his hand to the others. “I\
    \ recognize you from the Clover Club.” He grins toothily and lowers his\
    \ weapon. “Naomi sends her regards.”"
  , "Curious, you ask what the gangsters are doing here. “Ms. O’Bannion had\
    \ us investigate the attack on her fiancé’s club,” he explains. “Turns out\
    \ there were some men in Arkham behind the whole thing. Some Bishop\
    \ fellow and his lackeys. We tailed them all the way to this dump.” Before\
    \ he can explain further, the all-too-familiar ratta-tat of a tommy gun\
    \ echoes across the hill. “That’ll be Vinny. Come on, boys!” He beckons to\
    \ the others to follow and runs up the hill. Shaking your head, you do the\
    \ same. These mobsters don’t know what they’re getting into."
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
  , MinusFive
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
  { campaignLogRecorded = setFromList [NoBroodEscapedIntoTheWild]
  }

instance HasTokenValue WhereDoomAwaits where
  getTokenValue iid tokenFace (WhereDoomAwaits attrs) = case tokenFace of
    Skull -> do
      isAltered <-
        selectAny
        $ LocationWithInvestigator (InvestigatorWithId iid)
        <> LocationWithTrait Altered
      if isAltered
        then pure $ toTokenValue attrs Skull 3 5
        else pure $ toTokenValue attrs Skull 1 2
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> do
      agendaId <- selectJust AnyAgenda
      agendaStep <- fieldMap
        AgendaSequence
        (AS.unAgendaStep . AS.agendaStep)
        agendaId
      pure $ TokenValue
        Tablet
        (if isEasyStandard attrs
          then NegativeModifier (if agendaStep == 2 then 4 else 2)
          else if agendaStep == 2 then AutoFailModifier else NegativeModifier 3
        )
    ElderThing -> pure $ TokenValue ElderThing (NegativeModifier 0) -- determined by an effect
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage WhereDoomAwaits where
  runMessage msg s@(WhereDoomAwaits attrs) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      s <$ if standalone then push (SetTokens standaloneTokens) else pure ()
    StandaloneSetup -> do
      pure
        . WhereDoomAwaits
        $ attrs
        & standaloneCampaignLogL
        .~ standaloneCampaignLog
    Setup -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.sethBishop]
        [ EncounterSet.WhereDoomAwaits
        , EncounterSet.Whippoorwills
        , EncounterSet.BeastThralls
        , EncounterSet.Dunwich
        , EncounterSet.Sorcery
        , EncounterSet.BishopsThralls
        , EncounterSet.StrikingFear
        , EncounterSet.AncientEvils
        , EncounterSet.ChillingCold
        ]

      useV1 <- getHasRecord TheInvestigatorsRestoredSilasBishop
      useV2 <- liftA2
        (||)
        (getHasRecord TheInvestigatorsFailedToRecoverTheNecronomicon)
        (getHasRecord TheNecronomiconWasStolen)

      let
        ascendingTheHill = case (useV1, useV2) of
          (True, _) -> Acts.ascendingTheHillV1
          (False, True) -> Acts.ascendingTheHillV2
          (False, False) -> Acts.ascendingTheHillV3

      naomiHasTheInvestigatorsBacks <- getHasRecord
        NaomiHasTheInvestigatorsBacks
      noBroodEscaped <- getHasRecord NoBroodEscapedIntoTheWild
      broodEscapedCount <- if noBroodEscaped
        then pure 0
        else getRecordCount BroodEscapedIntoTheWild
      silasBishopPutOutOfMisery <- getHasRecord
        TheInvestigatorsPutSilasBishopOutOfHisMisery

      baseOfTheHill <- genCard Locations.baseOfTheHill
      ascendingPath <- genCard Locations.ascendingPath
      sentinelPeak <- genCard Locations.sentinelPeak

      silasMsgs <- if silasBishopPutOutOfMisery
        then do
          result <- gatherEncounterSet EncounterSet.HideousAbominations
          let
            conglomerationOfSpheres = fromJust . headMay $ result
            rest = drop 1 result
          pure
            [ SpawnEnemyAt
              (EncounterCard conglomerationOfSpheres)
              (toLocationId ascendingPath)
            , ShuffleCardsIntoDeck Deck.EncounterDeck $ map EncounterCard rest
            ]
        else pure []

      divergingPaths <- traverse genCard . take 3 =<< shuffleM
        [ Locations.slaughteredWoods
        , Locations.eerieGlade
        , Locations.destroyedPath
        , Locations.frozenSpring
        ]

      alteredPaths <- traverse genCard . take 3 =<< shuffleM
        [ Locations.dimensionalGap
        , Locations.aTearInThePath
        , Locations.uprootedWoods
        , Locations.lostMemories
        ]

      let
        inPlayLocations = [baseOfTheHill, ascendingPath, sentinelPeak]
        token = case scenarioDifficulty attrs of
          Easy -> MinusThree
          Standard -> MinusFive
          Hard -> MinusSix
          Expert -> MinusSeven

      pushAll
        $ story investigatorIds whereDoomAwaitsIntro
        : [ story investigatorIds whereDoomAwaitsPart1
          | naomiHasTheInvestigatorsBacks
          ]
        <> [ GainClues leadInvestigatorId 1 | naomiHasTheInvestigatorsBacks ]
        <> [ AddToken token
           , SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           ]
        <> replicate broodEscapedCount PlaceDoomOnAgenda
        <> [ PlaceLocation card | card <- inPlayLocations ]
        <> silasMsgs
        <> [ RevealLocation Nothing $ toLocationId baseOfTheHill
           , MoveAllTo (toSource attrs) $ toLocationId baseOfTheHill
           ]

      setAsideCards <- traverse genCard [Enemies.silasBishop]

      WhereDoomAwaits <$> runMessage
        msg
        (attrs
        & (actStackL
          . at 1
          ?~ [Acts.thePathToTheHill, ascendingTheHill, Acts.theGateOpens]
          )
        & (agendaStackL
          . at 1
          ?~ [Agendas.callingForthTheOldOnes, Agendas.beckoningForPower]
          )
        & (setAsideCardsL
          <>~ (divergingPaths <> alteredPaths <> setAsideCards)
          )
        )
    ResolveToken drawnToken Cultist iid -> s <$ pushAll
      [ CreateWindowModifierEffect
        EffectSkillTestWindow
        (EffectModifiers $ toModifiers attrs [CancelSkills])
        (TokenSource drawnToken)
        SkillTestTarget
      , CancelSkillEffects
      , DrawAnotherToken iid
      ]
    ResolveToken drawnToken ElderThing iid -> s <$ push
      (DiscardTopOfDeck
        iid
        (if isEasyStandard attrs then 2 else 3)
        (Just $ TokenTarget drawnToken)
      )
    DiscardedTopOfDeck _iid cards target@(TokenTarget token) ->
      s <$ case tokenFace token of
        ElderThing -> do
          let
            n = sum $ map
              (toPrintedCost . fromMaybe (StaticCost 0) . cdCost . toCardDef)
              cards
          push $ CreateTokenValueEffect (-n) (toSource attrs) target
        _ -> pure ()
    ScenarioResolution NoResolution ->
      s <$ push (ScenarioResolution $ Resolution 2)
    ScenarioResolution (Resolution 1) -> do
      xp <- getXp
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 1")
                 [ "The poorly bound tome appears to be the\
                   \ written records of Old Whateley, the man who taught Wilbur\
                   \ the ancient secrets of sorcery. You find a passage describing a\
                   \ place outside of time and space, where worlds converge and\
                   \ Yog-Sothoth dwells. Only by reaching this nexus at the edge of\
                   \ reality can you unmake the tear that has split open the world.\
                   \ Feeling as if you may be going to your doom, you muster a\
                   \ final ounce of courage and step into the gate."
                 ]
               ]
           ]
         , Record TheInvestigatorsEnteredTheGate
         ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 2) -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 2")
                 [ "The sorcerers from Dunwich, seeking arcane\
                        \ power from beyond this realm, have accomplished what\
                        \ Wilbur and Old Whateley could not. Through blood sacrifice\
                        \ and indescribable experiments, the dark power the sorcerers\
                        \ sought is now within their reach. However, they will never get\
                        \ the chance to truly wield this power. In beseeching Wilbur’s\
                        \ father for knowledge, they have drawn the creature forth\
                        \ from its extradimensional realm. Yog-Sothoth emerges from\
                        \ the open rift above Sentinel Hill, blotting out the sky and\
                        \ enveloping the world. Now it has come to Earth, and it rules\
                        \ where humanity once tread."
                 ]
               ]
           ]
         , Record
           YogSothothToreApartTheBarrierBetweenWorldsAndBecameOneWithAllReality
         ]
        <> [ DrivenInsane iid | iid <- investigatorIds ]
        <> [GameOver]
        )
    PlacedLocation name _ lid -> do
      when (name == "Altered Path") $ do
        alteredCount <- selectCount $ LocationWithTrait Altered
        push (SetLocationLabel lid $ "alteredPath" <> tshow alteredCount)
      when (name == "Diverging Path") $ do
        woodsCount <- selectCount $ LocationWithTrait Woods
        push (SetLocationLabel lid $ "divergingPath" <> tshow woodsCount)
      pure s
    _ -> WhereDoomAwaits <$> runMessage msg attrs
