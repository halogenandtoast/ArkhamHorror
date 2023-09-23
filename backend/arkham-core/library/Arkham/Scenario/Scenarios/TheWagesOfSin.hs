module Arkham.Scenario.Scenarios.TheWagesOfSin (
  TheWagesOfSin (..),
  theWagesOfSin,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Act
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheWagesOfSin.Story
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

instance HasChaosTokenValue TheWagesOfSin where
  getChaosTokenValue iid chaosTokenFace (TheWagesOfSin attrs) = case chaosTokenFace of
    Skull -> do
      n <- selectCount $ VictoryDisplayCardMatch $ CardWithTitle "Unfinished Business"
      pure $ toChaosTokenValue attrs Skull (n + 1) n
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
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
    SetChaosTokensForScenario -> do
      whenM getIsStandalone $ push (SetChaosTokens standaloneChaosTokens)
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
      spectralWebs <- genCards (replicate 4 Assets.spectralWeb)
      heretics <-
        genCards
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

      pushAll
        $ [ SetEncounterDeck $ Deck encounterDeck
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
    ResolveChaosToken _ tokenFace iid -> do
      case tokenFace of
        Skull | isHardExpert attrs -> push $ DrawAnotherChaosToken iid
        Tablet -> do
          heretics <- selectList $ EnemyWithTitle "Heretic"
          for_ heretics $ \heretic -> do
            push $ roundModifiers (ChaosTokenEffectSource Tablet) heretic [EnemyFight 1, EnemyEvade 1]
        ElderThing | isHardExpert attrs -> do
          mAction <- getSkillTestAction
          when (maybe False (`elem` [Action.Fight, Action.Evade]) mAction) $ do
            runHauntedAbilities iid
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Tablet -> do
          abilities <-
            selectList
              $ AbilityOnStory (StoryWithTitle "Unfinished Business" <> StoryWithPlacement (InThreatArea iid))
              <> AbilityIsForcedAbility
          unless (null abilities) $ do
            push $ chooseOne iid [AbilityLabel iid ability [] [] | ability <- abilities]
        ElderThing | isEasyStandard attrs -> do
          mAction <- getSkillTestAction
          when (maybe False (`elem` [Action.Fight, Action.Evade]) mAction) $ do
            runHauntedAbilities iid
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> do
      case resolution of
        NoResolution -> do
          anyResigned <- selectAny ResignedInvestigator
          push $ if anyResigned then R1 else R2
        Resolution res | res `elem` [1, 2] -> do
          iids <- allInvestigatorIds
          step <- getCurrentActStep
          n <- if step == 1 then pure 4 else selectCount $ EnemyWithTitle "Heretic"
          xp <- toGainXp attrs getXp
          pushAll
            $ story iids (if res == 1 then resolution1 else resolution2)
            : [Record TheInvestigatorsSurvivedTheWatchersEmbrace | res == 2]
              <> [RecordCount HereticsWereUnleashedUntoArkham n]
              <> [recordSetInsert MementosDiscovered [WispOfSpectralMist] | n <= 3]
              <> xp
        _ -> error "invalid resolution"
      pure s
    _ -> TheWagesOfSin <$> runMessage msg attrs
