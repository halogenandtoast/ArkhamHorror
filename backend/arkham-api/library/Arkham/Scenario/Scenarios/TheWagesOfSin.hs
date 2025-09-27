module Arkham.Scenario.Scenarios.TheWagesOfSin (setupTheWagesOfSin, theWagesOfSin, TheWagesOfSin (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Act
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers hiding (roundModifiers)
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Trait (Trait (Spectral))

newtype TheWagesOfSin = TheWagesOfSin ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheWagesOfSin where
  getModifiersFor (TheWagesOfSin a) = do
    modifySelect
      a
      (InvestigatorAt $ LocationWithTrait Spectral)
      [UseEncounterDeck SpectralEncounterDeck]
    spectral <- findAllCards (`cardMatch` CardWithTrait Spectral)
    modifyEach a (map (CardIdTarget . toCardId) spectral) [UseEncounterDeck SpectralEncounterDeck]

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
      n <- selectCount $ VictoryDisplayCardMatch $ basic $ CardWithTitle "Unfinished Business"
      pure $ toChaosTokenValue attrs Skull (n + 1) n
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

{- FOURMOLU_DISABLE -}
standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

setupTheWagesOfSin :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheWagesOfSin _attrs = do
  setup $ ul do
    li "gatherSets"
    li "theWatcherSet"
    li.nested "placeLocations" do
      li "spectral"
      li "startAt"
    li "setAside"
    li "heretics"
    li.nested "encounterDecks" do
      li "spectralDeck"
      li "standardDeck"

  scope "theSpectralEncounterDeck" $ flavor $ h "title" >> p "body"

  whenReturnTo $ gather Set.ReturnToTheWagesOfSin
  gather Set.TheWagesOfSin
  gather Set.AnettesCoven
  gather Set.CityOfSins `orWhenReturnTo` gather Set.CityOfTheDamned
  gather Set.InexorableFate `orWhenReturnTo` gather Set.UnspeakableFate
  gather Set.RealmOfDeath `orWhenReturnTo` gather Set.UnstableRealm
  gather Set.TrappedSpirits `orWhenReturnTo` gather Set.BloodthirstySpirits
  gather Set.Witchcraft `orWhenReturnTo` gather Set.Hexcraft
  gatherAndSetAside Set.TheWatcher

  whenReturnTo $ setAside [Assets.erynnMacAoidhDevotedEnchantress]

  setAgendaDeck [Agendas.theHangedManXII, Agendas.deathsApproach]

  isReturnTo <- getIsReturnTo
  erynnWantsToMeet <- getHasRecord ErynnWantsToMeet
  setActDeck
    $ if isReturnTo && erynnWantsToMeet
      then [Acts.inPursuitOfAnswers, Acts.inPursuitOfTheLiving, Acts.inPursuitOfTheBeyond]
      else [Acts.inPursuitOfTheDead, Acts.inPursuitOfTheLiving]

  heretics <-
    pickN 4
      $ [ Enemies.heretic_A
        , Enemies.heretic_C
        , Enemies.heretic_E
        , Enemies.heretic_G
        , Enemies.heretic_I
        , Enemies.heretic_K
        ]
      <> (guard isReturnTo *> [Enemies.returnToHeretic_38, Enemies.returnToHeretic_39])

  setAside $ replicate 4 Assets.spectralWeb <> heretics

  -- The locations are all "single-sided" because we need to handle the
  -- spectral state separately and therefor have no "unrevealed". So we
  -- need to exclude them here
  setExtraEncounterDeck SpectralEncounterDeck
    =<< amongGathered (CardWithTrait Spectral <> not_ #location)

  hangmansBrook <- Locations.hangmansBrook `orSampleIfReturnTo` [Locations.returnToHangmansBrook]
  startAt =<< place hangmansBrook

  placeOneOf_ (Locations.theGallows_169, Locations.theGallows_170)
  placeOneOf_ (Locations.hereticsGraves_171, Locations.hereticsGraves_172)
  placeOneOf_ (Locations.chapelAttic_175, Locations.chapelAttic_176)
  placeOneOf_ (Locations.chapelCrypt_173, Locations.chapelCrypt_174)
  placeAll
    [ Locations.hauntedFields
    , Locations.abandonedChapel
    ]

  whenReturnTo $ addAdditionalReferences ["54034b"]

instance RunMessage TheWagesOfSin where
  runMessage msg s@(TheWagesOfSin attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup TheWagesOfSin attrs $ setupTheWagesOfSin attrs
    ResolveChaosToken _ Skull iid -> do
      when (isHardExpert attrs) $ drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet _ -> do
      heretics <- select $ EnemyWithTitle "Heretic"
      for_ heretics $ \heretic -> roundModifiers Tablet heretic [EnemyFight 1, EnemyEvade 1]
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      when (isHardExpert attrs) do
        mAction <- getSkillTestAction
        when (maybe False (`elem` [Action.Fight, Action.Evade]) mAction) $ do
          runHauntedAbilities iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Tablet -> do
          abilities <-
            select
              $ AbilityOnStory (StoryWithTitle "Unfinished Business" <> StoryWithPlacement (InThreatArea iid))
              <> AbilityIsForcedAbility
          unless (null abilities) $ do
            chooseOne iid [AbilityLabel iid ability [] [] [] | ability <- abilities]
        ElderThing | isEasyStandard attrs -> do
          mAction <- getSkillTestAction
          when (maybe False (`elem` [#fight, #evade]) mAction) $ runHauntedAbilities iid
        _ -> pure ()
      pure s
    ScenarioResolution r -> do
      case r of
        NoResolution -> do
          anyResigned <- selectAny ResignedInvestigator
          push $ if anyResigned then R1 else R2
        Resolution res | res `elem` [1, 2] -> do
          step <- getCurrentActStep
          resolutionWithXp (if res == 1 then "resolution1" else "resolution2") $ allGainXp' attrs
          recordWhen (res == 2) TheInvestigatorsSurvivedTheWatchersEmbrace

          n <- if step == 1 then pure 4 else selectCount $ EnemyWithTitle "Heretic"
          recordCount HereticsWereUnleashedUntoArkham n
          when (n <= 3) $ recordSetInsert MementosDiscovered [WispOfSpectralMist]
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheWagesOfSin <$> liftRunMessage msg attrs
