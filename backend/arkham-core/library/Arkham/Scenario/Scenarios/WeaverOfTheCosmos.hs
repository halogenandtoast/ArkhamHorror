{-# LANGUAGE MultiWayIf #-}

module Arkham.Scenario.Scenarios.WeaverOfTheCosmos (
  WeaverOfTheCosmos (..),
  weaverOfTheCosmos,
) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Attack
import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Difficulty
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Log (getRecordCount)
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTarget)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (AncientOne, Spider))
import Arkham.Treachery.Cards qualified as Treacheries

newtype WeaverOfTheCosmos = WeaverOfTheCosmos ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

weaverOfTheCosmos :: Difficulty -> WeaverOfTheCosmos
weaverOfTheCosmos difficulty =
  scenario
    WeaverOfTheCosmos
    "06333"
    "Weaver of the Cosmos"
    difficulty
    [ "theGreatWeb1"
    , "theGreatWeb2"
    , "theGreatWeb3"
    , "theGreatWeb4"
    ]

instance HasChaosTokenValue WeaverOfTheCosmos where
  getChaosTokenValue iid tokenFace (WeaverOfTheCosmos attrs) = case tokenFace of
    Skull -> do
      maxDoom <- fieldMax LocationDoom Anywhere
      totalDoom <- selectSum LocationDoom Anywhere
      pure $ toChaosTokenValue attrs Skull maxDoom totalDoom
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet $ byDifficulty attrs ZeroModifier (NegativeModifier 1)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
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
  , MinusFour
  , Skull
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage WeaverOfTheCosmos where
  runMessage msg s@(WeaverOfTheCosmos attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "dreamEaters.weaverOfTheCosmos.intro"
      pure s
    StandaloneSetup -> do
      record RandolphDidNotSurviveTheDescent
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup WeaverOfTheCosmos attrs do
      gather Set.WeaverOfTheCosmos
      gather Set.AgentsOfAtlachNacha
      gather Set.Spiders
      gather Set.AncientEvils

      setAgendaDeck [Agendas.theBridgeOfWebs, Agendas.aTrailOfTwists, Agendas.realitiesInterwoven]
      setActDeck [Acts.journeyAcrossTheBridge, Acts.theWeaverOfTheCosmos, Acts.theSchemesDemise]

      (startingGreatWebs, setAsideGreatWebs) <-
        splitAt 4
          <$> shuffleM
            [ Locations.theGreatWebWebStairs
            , Locations.theGreatWebWebStairs
            , Locations.theGreatWebWebStairs
            , Locations.theGreatWebCosmicWeb
            , Locations.theGreatWebCosmicWeb
            , Locations.theGreatWebTangledWeb
            , Locations.theGreatWebTangledWeb
            , Locations.theGreatWebTangledWeb
            , Locations.theGreatWebPrisonOfCocoons
            , Locations.theGreatWebPrisonOfCocoons
            , Locations.theGreatWebVastWeb
            , Locations.theGreatWebVastWeb
            , Locations.theGreatWebWebWovenIsland
            , Locations.theGreatWebWebWovenIsland
            , Locations.theGreatWebWebWovenIsland
            ]

      theGreatWebs <- placeLabeledLocations "theGreatWeb" =<< genCards startingGreatWebs
      pushAll
        [ PlacedLocationDirection l1 Above l2
        | (l1, l2) <- zip theGreatWebs (drop 1 theGreatWebs)
        ]

      case theGreatWebs of
        [top, _, _, bottom] -> do
          startAt top

          n <- getRecordCount StepsOfTheBridge

          when (n >= 3 && n <= 5) do
            placeTokens attrs bottom #doom 1

          when (n >= 6 && n <= 8) do
            placeTokens attrs bottom #doom 2

          when (n >= 9 && n <= 11) do
            placeTokens attrs bottom #doom 3

          when (n >= 12) do
            placeTokens attrs bottom #doom 4
        _ -> error "Wrong number of webs"

      setAside
        $ setAsideGreatWebs
        <> [ Enemies.atlachNacha
           , Enemies.legsOfAtlachNacha_347
           , Enemies.legsOfAtlachNacha_348
           , Enemies.legsOfAtlachNacha_349
           , Enemies.legsOfAtlachNacha_350
           , Treacheries.theSpinnerInDarkness
           ]
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> do
          mEnemy <- selectOne $ enemyAtLocationWith iid <> EnemyWithTrait AncientOne
          for_ mEnemy \enemy ->
            push $ InitiateEnemyAttack $ enemyAttack enemy CultistEffect iid
        ElderThing -> void $ runMaybeT do
          Action.Fight <- MaybeT getSkillTestAction
          EnemyTarget eid <- MaybeT getSkillTestTarget
          guardM (lift $ eid <=~> EnemyWithTrait Spider)
          lid <- MaybeT $ getMaybeLocation iid
          lift $ placeTokens ElderThingEffect lid #doom 1
        _ -> pure ()
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Tablet | n >= 2 -> do
          mlid <- getMaybeLocation iid
          for_ mlid \lid -> removeTokens TabletEffect lid #doom 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> do
      case r of
        NoResolution -> do
          story $ i18nWithTitle "dreamEaters.weaverOfTheCosmos.noResolution"
          record TheBridgeWasCompleted
          eachInvestigator $ kill attrs
          whenM getIsTheWebOfDreams $ push GameOver
          endOfScenario
        Resolution 1 -> do
          story $ i18n "dreamEaters.weaverOfTheCosmos.resolution1"
          record TheBridgeWasDestroyed
          allGainXpWithBonus attrs 5
          foundAWayOut <- getHasRecord TheInvestigatorsFoundAWayOutOfTheUnderworld
          trapped <- getHasRecord TheInvestigatorsAreTrappedInAtlachNacha'sRealm
          if
            | foundAWayOut -> push R3
            | trapped -> push R4
            | otherwise -> push R5
        Resolution 2 -> do
          story $ i18n "dreamEaters.weaverOfTheCosmos.resolution2"
          record TheBridgeWasCompleted
          eachInvestigator $ push . DrivenInsane
          whenM getIsTheWebOfDreams $ push GameOver
          endOfScenario
        Resolution 3 -> do
          story $ i18n "dreamEaters.weaverOfTheCosmos.resolution3"
          record TheInvestigatorsReturnedToReality
          eachInvestigator (`sufferPhysicalTrauma` 2)
          whenM getIsTheDreamQuest $ push GameOver
          endOfScenario
        Resolution 4 -> do
          story $ i18n "dreamEaters.weaverOfTheCosmos.resolution4"
          record TheInvestigatorsNeverEscaped
          eachInvestigator $ push . DrivenInsane
          whenM getIsTheDreamQuest $ push GameOver
          endOfScenario
        Resolution 5 -> do
          story $ i18n "dreamEaters.weaverOfTheCosmos.resolution5"
          record TheInvestigatorsAreStillInTheDreamlands
          eachInvestigator (`sufferPhysicalTrauma` 2)
          whenM getIsTheDreamQuest $ push GameOver
          endOfScenario
        other -> throw $ UnknownResolution other

      pure s
    _ -> WeaverOfTheCosmos <$> liftRunMessage msg attrs
