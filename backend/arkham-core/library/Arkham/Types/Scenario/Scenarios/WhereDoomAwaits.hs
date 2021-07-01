module Arkham.Types.Scenario.Scenarios.WhereDoomAwaits
  ( WhereDoomAwaits(..)
  , whereDoomAwaits
  )
where

import Arkham.Prelude

import Arkham.Types.AgendaId
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait hiding (Cultist, Expert)
import Data.Maybe (fromJust)

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
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance HasRecord WhereDoomAwaits where
  hasRecord NaomiHasTheInvestigatorsBacks = pure False
  hasRecord TheInvestigatorsPutSilasBishopOutOfHisMisery = pure False
  hasRecord NoBroodEscapedIntoTheWild = pure True
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance
  ( HasTokenValue env InvestigatorId
  , HasStep env AgendaStep
  , HasSet Trait env LocationId
  , HasId LocationId env InvestigatorId
  )
  => HasTokenValue env WhereDoomAwaits where
  getTokenValue (WhereDoomAwaits attrs) iid = \case
    Skull -> do
      lid <- getId @LocationId iid
      isAltered <- member Altered <$> getSet lid
      if isAltered
        then pure $ toTokenValue attrs Skull 3 5
        else pure $ toTokenValue attrs Skull 1 2
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> do
      agendaStep <- unAgendaStep <$> getStep
      pure $ TokenValue
        Tablet
        (if isEasyStandard attrs
          then NegativeModifier (if agendaStep == 2 then 4 else 2)
          else if agendaStep == 2 then AutoFailModifier else NegativeModifier 3
        )
    ElderThing -> pure $ TokenValue ElderThing (NegativeModifier 0) -- determined by an effect
    otherFace -> getTokenValue attrs iid otherFace

instance
  ( HasCount XPCount env ()
  , HasId (Maybe LocationId) env LocationMatcher
  , HasSet InvestigatorId env ()
  , HasSet LocationId env [Trait]
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
      encounterDeck <- buildEncounterDeckExcluding
        ["02293"]
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

      naomiHasTheInvestigatorsBacks <- getHasRecord
        NaomiHasTheInvestigatorsBacks
      noBroodEscaped <- getHasRecord NoBroodEscapedIntoTheWild
      broodEscapedCount <- if noBroodEscaped
        then pure 0
        else getRecordCount BroodEscapedIntoTheWild
      silasBishopPutOutOfMisery <- getHasRecord
        TheInvestigatorsPutSilasBishopOutOfHisMisery

      baseOfTheHillId <- getRandom
      ascendingPathId <- getRandom
      sentinelPeakId <- getRandom

      silasMsgs <- if silasBishopPutOutOfMisery
        then do
          result <- EncounterSet.gatherEncounterSet
            EncounterSet.HideousAbominations
          let
            conglomerationOfSpheres = fromJust . headMay $ result
            rest = drop 1 result
          pure
            [ SpawnEnemyAt
              (EncounterCard conglomerationOfSpheres)
              ascendingPathId
            , ShuffleIntoEncounterDeck rest
            ]
        else pure []

      divergingPaths <- take 3 <$> shuffleM ["02285", "02286", "02287", "02288"]
      alteredPaths <- take 3 <$> shuffleM ["02289", "02290", "02291", "02292"]

      let
        inPlayLocations =
          [ (baseOfTheHillId, "02282")
          , (ascendingPathId, "02283")
          , (sentinelPeakId, "02284")
          ]
        locations = map snd inPlayLocations <> divergingPaths <> alteredPaths
        locations' = unionsWith (<>) $ map
          (uncurry singletonMap . second pure . toFst
            (getLocationName . lookupLocationStub)
          )
          locations
        token = case scenarioDifficulty attrs of
          Easy -> MinusThree
          Standard -> MinusFive
          Hard -> MinusSix
          Expert -> MinusSeven

      unshiftMessages
        $ story investigatorIds whereDoomAwaitsIntro
        : [ story investigatorIds whereDoomAwaitsPart1
          | naomiHasTheInvestigatorsBacks
          ]
        <> [ GainClues leadInvestigatorId 1 | naomiHasTheInvestigatorsBacks ]
        <> [ AddToken token
           , SetEncounterDeck encounterDeck
           , AddAgenda "02275"
           , AddAct "02277"
           ]
        <> replicate broodEscapedCount PlaceDoomOnAgenda
        <> [ PlaceLocation cardCode locationId
           | (locationId, cardCode) <- inPlayLocations
           ]
        <> silasMsgs
        <> [RevealLocation Nothing baseOfTheHillId, MoveAllTo baseOfTheHillId]

      WhereDoomAwaits <$> runMessage
        msg
        (attrs
        & (locationsL .~ locations')
        & (setAsideLocationsL .~ (divergingPaths <> alteredPaths))
        )
    ResolveToken drawnToken Cultist iid -> s <$ unshiftMessages
      [ CreateWindowModifierEffect
        EffectSkillTestWindow
        (EffectModifiers $ toModifiers attrs [CancelSkills])
        (DrawnTokenSource drawnToken)
        SkillTestTarget
      , CancelSkillEffects
      , DrawAnotherToken iid
      ]
    ResolveToken drawnToken ElderThing iid -> s <$ unshiftMessage
      (DiscardTopOfDeck
        iid
        (if isEasyStandard attrs then 2 else 3)
        (Just $ DrawnTokenTarget drawnToken)
      )
    DiscardedTopOfDeck _iid cards target@(DrawnTokenTarget token) ->
      s <$ case drawnTokenFace token of
        ElderThing -> do
          let n = sum $ map (toPrintedCost . cdCost . pcDef) cards
          unshiftMessage $ CreateTokenValueEffect (-n) (toSource attrs) target
        _ -> pure ()
    ScenarioResolution NoResolution ->
      s <$ unshiftMessage (ScenarioResolution $ Resolution 2)
    ScenarioResolution (Resolution 1) -> do
      xp <- getXp
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
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
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    ScenarioResolution (Resolution 2) -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
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
        alteredCount <- length <$> getSetList @LocationId [Woods]
        unshiftMessage
          (SetLocationLabel lid $ "alteredPath" <> tshow alteredCount)
      when (name == "Diverging Path") $ do
        woodsCount <- length <$> getSetList @LocationId [Woods]
        unshiftMessage
          (SetLocationLabel lid $ "divergingPath" <> tshow woodsCount)
      pure s
    _ -> WhereDoomAwaits <$> runMessage msg attrs
