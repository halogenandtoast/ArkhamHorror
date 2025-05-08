module Arkham.Scenario.Scenarios.LostInTimeAndSpace (lostInTimeAndSpace, LostInTimeAndSpace (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Act
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.LostInTimeAndSpace.Helpers
import Arkham.Trait hiding (Cultist, ElderThing)

newtype LostInTimeAndSpace = LostInTimeAndSpace ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTimeAndSpace :: Difficulty -> LostInTimeAndSpace
lostInTimeAndSpace difficulty =
  scenario
    LostInTimeAndSpace
    "02311"
    "Lost in Time and Space"
    difficulty
    scenarioLayout

instance HasChaosTokenValue LostInTimeAndSpace where
  getChaosTokenValue iid chaosTokenFace (LostInTimeAndSpace attrs) = case chaosTokenFace of
    Skull -> do
      extradimensionalCount <- selectCount $ LocationWithTrait Extradimensional
      pure
        $ ChaosTokenValue Skull
        $ NegativeModifier
        $ if isEasyStandard attrs then min extradimensionalCount 5 else extradimensionalCount
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> do
      shroud <- maybe (pure 0) (fieldWithDefault 0 LocationShroud) =<< field InvestigatorLocation iid
      pure $ toChaosTokenValue attrs ElderThing shroud (shroud * 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
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

readInvestigatorDefeat :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> m ()
readInvestigatorDefeat a = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    storyOnly' defeated "defeated"
    for_ defeated (kill a)

instance RunMessage LostInTimeAndSpace where
  runMessage msg s@(LostInTimeAndSpace attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" do
        h "title"
        p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup LostInTimeAndSpace attrs do
      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li "setAside"
          unscoped $ li "shuffleRemainder"

      scope "locationsInTheEncounterDeck" $ flavor do
        setTitle "title"
        p "body"

      gather Set.LostInTimeAndSpace
      gather Set.Sorcery
      gather Set.TheBeyond
      gather Set.HideousAbominations
      gather Set.AgentsOfYogSothoth

      startAt =<< place Locations.anotherDimension

      setAside
        [ Locations.theEdgeOfTheUniverse
        , Locations.tearThroughTime
        , Enemies.yogSothoth
        ]
      setActDeck
        [ Acts.outOfThisWorld
        , Acts.intoTheBeyond
        , Acts.closeTheRift
        , Acts.findingANewWay
        ]
      setAgendaDeck
        [ Agendas.allIsOne
        , Agendas.pastPresentAndFuture
        , Agendas.breakingThrough
        , Agendas.theEndOfAllThings
        ]
    After (PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _) -> do
      case (isHardExpert attrs, chaosTokenFace token) of
        (True, Cultist) -> discardUntilFirst iid Cultist Deck.EncounterDeck (basic #location)
        (_, Tablet) -> do
          mYogSothothId <- selectOne (EnemyWithTitle "Yog-Sothoth")
          for_ mYogSothothId $ \eid -> initiateEnemyAttack eid attrs iid
        _ -> pure ()
      pure s
    After (FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _) -> do
      case token.face of
        Cultist -> discardUntilFirst iid Cultist Deck.EncounterDeck (basic #location)
        Tablet -> do
          mYogSothothId <- selectOne (EnemyWithTitle "Yog-Sothoth")
          for_ mYogSothothId $ \eid -> initiateEnemyAttack eid attrs iid
        ElderThing -> withLocationOf iid \lid -> do
          whenM (lid <=~> LocationWithTrait Extradimensional) $ toDiscardBy iid ElderThing lid
        _ -> pure ()
      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    RequestedEncounterCard (ChaosTokenEffectSource Cultist) (Just iid) (Just card) -> do
      moveTo attrs iid =<< placeLocation (EncounterCard card)
      pure s
    ScenarioResolution r -> scope "resolutions" do
      readInvestigatorDefeat attrs
      case r of
        NoResolution -> do
          step <- getCurrentActStep
          do_ $ if step == 4 then R2 else R4
        _ -> do_ msg
      pure s
    Do (ScenarioResolution r) -> scope "resolutions" do
      case r of
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 5
          record TheInvestigatorsClosedTheTearInReality
          eachInvestigator \iid -> sufferTrauma iid 2 2
        Resolution 2 -> do
          resolution "resolution2"
        Resolution 3 -> do
          resolution "resolution3"
          record YogSothothHasFledToAnotherDimension
          eachInvestigator (kill attrs)
        Resolution 4 -> do
          resolution "resolution4"
          record YogSothothToreApartTheBarrierBetweenWorldsAndBecameOneWithAllReality
          eachInvestigator drivenInsane
        other -> throwIO $ UnknownResolution other
      endOfScenario
      pure s
    _ -> LostInTimeAndSpace <$> liftRunMessage msg attrs
