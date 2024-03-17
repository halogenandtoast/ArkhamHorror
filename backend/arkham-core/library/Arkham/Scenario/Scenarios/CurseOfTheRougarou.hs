module Arkham.Scenario.Scenarios.CurseOfTheRougarou (CurseOfTheRougarou (..), curseOfTheRougarou) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice)
import Arkham.Scenario.Runner hiding (chooseOne, story)
import Arkham.Scenario.Setup
import Arkham.Scenarios.CurseOfTheRougarou.FlavorText
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Trait hiding (Cultist)
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Maybe (fromJust)

newtype CurseOfTheRougarou = CurseOfTheRougarou ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

curseOfTheRougarou :: Difficulty -> CurseOfTheRougarou
curseOfTheRougarou difficulty =
  scenario
    CurseOfTheRougarou
    "81001"
    "Curse of the Rougarou"
    difficulty
    [ "     .       unhallowed1      newOrleans1       ."
    , "unhallowed2 unhallowedBayou newOrleansBayou newOrleans2"
    , "riverside2 riversideBayou wildernessBayou wilderness2"
    , "     .       riverside1      wilderness1       ."
    ]

instance HasChaosTokenValue CurseOfTheRougarou where
  getChaosTokenValue iid chaosTokenFace (CurseOfTheRougarou attrs) = case chaosTokenFace of
    Skull -> do
      isBayou <- selectAny $ LocationWithTrait Bayou <> locationWithInvestigator iid
      pure
        $ if isBayou
          then toChaosTokenValue attrs Skull 4 6
          else toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ ChaosTokenValue Tablet ZeroModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 4)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage CurseOfTheRougarou where
  runMessage msg s@(CurseOfTheRougarou attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro
      pure s
    Setup -> runScenarioSetup CurseOfTheRougarou attrs $ do
      gather EncounterSet.TheBayou

      result <- shuffleM $ keys locationsByTrait
      let
        trait = fromJust . headMay . drop 1 $ result
        rest = drop 2 result

      setAside [Assets.ladyEsprit, Assets.bearTrap, Assets.fishingNet]
      setAside $ concatMap (\t -> findWithDefault [] t locationsByTrait) rest

      startingLocationsWithLabel <- locationsWithLabels trait $ findWithDefault [] trait locationsByTrait

      let
        ((bayouLabel, bayou), others) =
          case break (elem Bayou . toTraits . snd) startingLocationsWithLabel of
            (as, x : bs) -> (x, as <> bs)
            _ -> error "handled"

      start <- place bayou
      startAt start
      push $ SetLocationLabel start bayouLabel

      for_ others $ \(label, card) -> do
        locationId <- place card
        push $ SetLocationLabel locationId label

      setAgendaDeck
        [ Agendas.aCreatureOfTheBayou
        , Agendas.theRougarouFeeds
        , Agendas.theCurseSpreads
        ]
      setActDeck [Acts.findingLadyEsprit, Acts.huntingTheRougarou]
    SetChaosTokensForScenario -> do
      let
        tokens =
          if isEasyStandard attrs
            then
              [ PlusOne
              , PlusOne
              , Zero
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
              , MinusFour
              , MinusFive
              , MinusSix
              , Skull
              , Skull
              , Cultist
              , Cultist
              , Tablet
              , ElderThing
              , AutoFail
              , ElderSign
              ]
            else
              [ PlusOne
              , Zero
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
              , MinusFour
              , MinusFive
              , MinusFive
              , MinusSix
              , MinusEight
              , Skull
              , Skull
              , Skull
              , Cultist
              , Cultist
              , Tablet
              , ElderThing
              , AutoFail
              , ElderSign
              ]
      s <$ push (SetChaosTokens tokens)
    ResolveChaosToken _ Cultist iid -> do
      rougarouAtYourLocation <- selectAny $ enemyIs Enemies.theRougarou <> enemyAtLocationWith iid
      pushWhen rougarouAtYourLocation (DrawAnotherChaosToken iid)
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      if isEasyStandard attrs
        then do
          mrougarou <- selectOne $ enemyIs Enemies.theRougarou <> enemyAtLocationWith iid
          for_ mrougarou \eid -> push $ EnemyWillAttack $ enemyAttack eid attrs iid
        else do
          lid <- getJustLocation iid
          connectedLocationIds <- select $ AccessibleFrom $ LocationWithId lid
          mrougarou <-
            selectOne
              $ enemyIs Enemies.theRougarou
              <> EnemyAt (oneOf $ map LocationWithId $ lid : connectedLocationIds)
          for_ mrougarou \eid -> push $ EnemyWillAttack $ enemyAttack eid attrs iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      pushWhen (token.face == Tablet) (roundModifier TabletEffect iid CannotMove)
      pure s
    ScenarioResolution NoResolution ->
      runMessage (ScenarioResolution $ Resolution 1) s
    ScenarioResolution (Resolution 1) -> do
      story resolution1
      record TheRougarouContinuesToHauntTheBayou
      allGainXp attrs
      endOfScenario
      pure s
    ScenarioResolution (Resolution 2) -> do
      lead <- getLead
      story resolution2
      record TheRougarouIsDestroyed
      removeCampaignCard Treacheries.curseOfTheRougarou
      addCampaignCardToDeckChoice [lead] Assets.ladyEsprit
      allGainXp attrs
      endOfScenario
      pure s
    ScenarioResolution (Resolution 3) -> do
      lead <- getLead
      story resolution3
      record TheRougarouEscapedAndYouEmbracedTheCurse
      addCampaignCardToDeck lead Assets.monstrousTransformation
      allGainXp attrs
      endOfScenario
      pure s
    _ -> CurseOfTheRougarou <$> lift (runMessage msg attrs)
