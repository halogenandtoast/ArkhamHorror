module Arkham.Scenario.Scenarios.TheWagesOfSin (
  TheWagesOfSin (..),
  theWagesOfSin,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheWagesOfSin.Story
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait (Trait (Spectral), toTraits)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TheWagesOfSin = TheWagesOfSin ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWagesOfSin :: Difficulty -> TheWagesOfSin
theWagesOfSin difficulty =
  scenario
    (TheWagesOfSin . (`with` Metadata mempty mempty))
    "05161"
    "The Wages of Sin"
    difficulty
    [ ".              theGallows    .             chapelAttic     ."
    , "hereticsGraves hauntedFields .             abandonedChapel chapelCrypt"
    , ".              .             hangmansBrook .               ."
    ]

instance HasTokenValue TheWagesOfSin where
  getTokenValue iid tokenFace (TheWagesOfSin attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
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

instance RunMessage TheWagesOfSin where
  runMessage msg s@(TheWagesOfSin attrs) = case msg of
    PreScenarioSetup -> do
      iids <- allInvestigatorIds
      push $ story iids intro
      pure s
    SetTokensForScenario -> do
      whenM getIsStandalone $ push (SetTokens standaloneTokens)
      pure s
    Setup -> do
      -- The locations are all "single-sided" because we need to handle the
      -- spectral state separately and therefor have no "unrevealed". So we
      -- need to exclude them here
      gatheredCards <-
        buildEncounterDeckExcludingMatching
          (CardWithType LocationType)
          [ EncounterSet.TheWagesOfSin
          , EncounterSet.AnettesCoven
          , EncounterSet.CityOfSins
          , EncounterSet.InexorableFate
          , EncounterSet.RealmOfDeath
          , EncounterSet.TrappedSpirits
          , EncounterSet.Witchcraft
          ]
      let (spectralEncounterDeck, encounterDeck) = partition (elem Spectral . toTraits) (unDeck gatheredCards)
      theWatcher <-
        map EncounterCard
          <$> gatherEncounterSet EncounterSet.TheWatcher
      spectralWebs <- traverse genCard (replicate 4 Assets.spectralWeb)
      heretics <-
        traverse genCard
          =<< sampleN
            4
            ( Enemies.heretic_A
                :| [ Enemies.heretic_C
                   , Enemies.heretic_E
                   , Enemies.heretic_G
                   , Enemies.heretic_I
                   , Enemies.heretic_K
                   ]
            )
      let setAsideCards = theWatcher <> spectralWebs <> heretics

      theGallows <-
        sample (Locations.theGallows_169 :| [Locations.theGallows_170])

      hereticsGraves <-
        sample (Locations.hereticsGraves_171 :| [Locations.hereticsGraves_172])

      chapelAttic <-
        sample (Locations.chapelAttic_175 :| [Locations.chapelAttic_176])

      chapelCrypt <-
        sample (Locations.chapelCrypt_173 :| [Locations.chapelCrypt_174])

      (hangmansBrookId, placeHangmansBrook) <- placeLocationCard Locations.hangmansBrook

      placements <-
        traverse
          placeLocationCard_
          [ theGallows
          , hereticsGraves
          , chapelAttic
          , chapelCrypt
          , Locations.hauntedFields
          , Locations.abandonedChapel
          ]

      pushAll $
        [ SetEncounterDeck $ Deck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        ]
          <> placements
          <> [placeHangmansBrook, MoveAllTo (toSource attrs) hangmansBrookId]

      agendas <- genCards [Agendas.theHangedManXII, Agendas.deathsApproach]
      acts <- genCards [Acts.inPursuitOfTheDead, Acts.inPursuitOfTheLiving]

      TheWagesOfSin . (`with` Metadata (Deck spectralEncounterDeck) mempty)
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
          )
    InvestigatorDoDrawEncounterCard iid -> do
      atSpectralLocation <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Spectral
      if atSpectralLocation
        then case unDeck (spectralEncounterDeck meta) of
          [] -> do
            when (notNull $ spectralDiscard meta) $ do
              pushAll
                [ShuffleEncounterDiscardBackIn, InvestigatorDrawEncounterCard iid]
            pure s
          -- This case should not happen but this safeguards against it
          (card : spectralDeck) -> do
            when (null spectralDeck) $ do
              windows' <-
                checkWindows
                  [Window Timing.When Window.EncounterDeckRunsOutOfCards]
              pushAll [windows', ShuffleEncounterDiscardBackIn]
            pushAll [UnsetActiveCard, InvestigatorDrewEncounterCard iid card]
            pure . TheWagesOfSin $ attrs `with` (meta {spectralEncounterDeck = Deck spectralDeck})
        else TheWagesOfSin . (`with` meta) <$> runMessage msg attrs
    _ -> do
      -- This is how we can let behaviors control the data for the scenario since
      -- ScenarioAttrs can't tell much about our data. The main mechanism for
      -- circumventing this is to use an IORef, but it does mean you need to get
      -- both your updated IORef value as well as the updated attrs
      let
        wagesOfSinBehaviors =
          ScenarioBehaviors
            { discardLens = \c ->
                pure $
                  if Spectral `elem` toTraits c
                    then encounterDecksL . at SpectralEncounterDeck . non (Deck [], []) . _2
                    else discardL
            , deckLens = \iid -> do
                atSpectralLocation <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Spectral
                pure $
                  if atSpectralLocation
                    then encounterDecksL . at SpectralEncounterDeck . non (Deck [], []) . _1
                    else encounterDeckL
            }

      result <- runScenarioAttrs msg wagesOfSinBehaviors attrs
      meta' <- readIORef metaRef
      pure $ TheWagesOfSin $ result `with` meta'
