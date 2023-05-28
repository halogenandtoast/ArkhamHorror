module Arkham.Scenario.Scenarios.TheWagesOfSin (
  TheWagesOfSin (..),
  theWagesOfSin,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Act
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheWagesOfSin.Story
import Arkham.Token
import Arkham.Trait (Trait (Spectral), toTraits)

newtype TheWagesOfSin = TheWagesOfSin ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheWagesOfSin where
  getModifiersFor (InvestigatorTarget iid) (TheWagesOfSin a) = do
    atSpectralLocation <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Spectral
    pure $ toModifiers a [UseEncounterDeck SpectralEncounterDeck | atSpectralLocation]
  getModifiersFor (CardIdTarget cid) (TheWagesOfSin a) = do
    isSpectral <- (`cardMatch` CardWithTrait Spectral) <$> getCard cid
    pure $ toModifiers a [UseEncounterDeck SpectralEncounterDeck | isSpectral]
  getModifiersFor _ _ = pure []

theWagesOfSin :: Difficulty -> TheWagesOfSin
theWagesOfSin difficulty =
  scenario
    TheWagesOfSin
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
          (CardWithOneOf [CardWithType LocationType, CardWithTitle "Heretic"])
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

      TheWagesOfSin
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
              & (encounterDecksL . at SpectralEncounterDeck ?~ (Deck spectralEncounterDeck, mempty))
          )
    ScenarioResolution resolution -> do
      case resolution of
        NoResolution -> do
          anyResigned <- selectAny ResignedInvestigator
          push $ scenarioResolution $ if anyResigned then 1 else 2
        Resolution res | res `elem` [1, 2] -> do
          iids <- allInvestigatorIds
          step <- getCurrentActStep
          n <- if step == 1 then pure 4 else selectCount $ EnemyWithTitle "Heretic"
          xp <- toGainXp attrs getXp
          pushAll $
            story iids (if res == 1 then resolution1 else resolution2)
              : [Record TheInvestigatorsSurvivedTheWatchersEmbrace | res == 2]
                <> [RecordCount HereticsWereUnleashedUntoArkham n]
                <> [recordSetInsert MementosDiscovered [WispOfSpectralMist] | n <= 3]
                <> xp
        _ -> error "invalid resolution"
      pure s
    _ -> TheWagesOfSin <$> runMessage msg attrs
