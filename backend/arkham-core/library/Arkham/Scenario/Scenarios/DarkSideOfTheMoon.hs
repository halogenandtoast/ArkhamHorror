module Arkham.Scenario.Scenarios.DarkSideOfTheMoon (DarkSideOfTheMoon (..), darkSideOfTheMoon) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Campaign (getCampaignStoryCard)
import Arkham.Helpers.Log (whenHasRecord)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Runner hiding (assignEnemyDamage, drawEncounterCard, story)
import Arkham.Scenario.Setup
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype DarkSideOfTheMoon = DarkSideOfTheMoon ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkSideOfTheMoon :: Difficulty -> DarkSideOfTheMoon
darkSideOfTheMoon difficulty =
  scenario
    DarkSideOfTheMoon
    "06206"
    "Dark Side of the Moon"
    difficulty
    [ "theWhiteShip          lightSideOfTheMoon cavernsBeneathTheMoonLightSide"
    , "cityOfTheMoonBeasts   theDarkCrater      theBlackCore"
    , "templeOfTheMoonLizard moonForest         cavernsBeneathTheMoonDarkSide"
    , ".                     moonBeastGalley    ."
    ]

instance HasChaosTokenValue DarkSideOfTheMoon where
  getChaosTokenValue iid tokenFace (DarkSideOfTheMoon attrs) = case tokenFace of
    Skull -> do
      alarmLevel <- getAlarmLevel iid
      pure $ toChaosTokenValue attrs Skull (alarmLevel + 1 `div` 2) alarmLevel
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ ChaosTokenValue ElderThing (byDifficulty attrs (PositiveModifier 1) (PositiveModifier 0))
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
  , Skull
  , Cultist
  , Tablet
  , Tablet
  , AutoFail
  , ElderSign
  ]

instance RunMessage DarkSideOfTheMoon where
  runMessage msg s@(DarkSideOfTheMoon attrs) = runQueueT $ case msg of
    StandaloneSetup -> do
      push $ SetChaosTokens standaloneChaosTokens
      pure s
    PreScenarioSetup -> do
      notCaptured <- selectAny $ not_ (InvestigatorWithRecord WasCaptured)
      captured <- selectAny $ InvestigatorWithRecord WasCaptured
      when captured $ story $ i18nWithTitle "dreamEaters.darkSideOfTheMoon.intro1"
      when notCaptured $ story $ i18nWithTitle "dreamEaters.darkSideOfTheMoon.intro2"
      pure s
    Setup -> runScenarioSetup DarkSideOfTheMoon attrs do
      gather Set.DarkSideOfTheMoon
      gather Set.Corsairs
      gather Set.DreamersCurse
      gather Set.AncientEvils

      setAgendaDeck [Agendas.silentStirring, Agendas.theAlarmIsRaised, Agendas.theyAreUponYou]
      setActDeck
        [Acts.inTheBellyOfTheMoonBeast, Acts.exploringTheMoon, Acts.theMoonsCore, Acts.unexpectedRescue]

      place_ Locations.cityOfTheMoonBeasts
      place_ Locations.templeOfTheMoonLizard
      moonForest <- place Locations.moonForest
      place_ Locations.theDarkCrater

      captured <- select $ InvestigatorWithRecord WasCaptured
      if notNull captured
        then do
          moonBeastGalley <- place Locations.moonBeastGalley
          for_ captured \iid -> do
            moveTo attrs iid moonBeastGalley
            placeClues attrs moonBeastGalley 2
        else setAside [Locations.moonBeastGalley]

      notCaptured <- select $ not_ (InvestigatorWithRecord WasCaptured)
      for_ notCaptured \iid -> moveTo attrs iid moonForest

      whenHasRecord RandolphWasCaptured do
        getCampaignStoryCard Assets.randolphCarterExpertDreamer >>= push . SetAsideCards . pure . toCard

      setAside
        [ Enemies.moonLizard
        , Assets.virgilGrayTrulyInspired
        , Assets.theCaptain
        , Treacheries.falseAwakening
        , Locations.cavernsBeneathTheMoonDarkSide
        , Locations.cavernsBeneathTheMoonLightSide
        , Locations.lightSideOfTheMoon
        , Locations.theBlackCore
        , Locations.theWhiteShip
        ]

      for_ (captured <> notCaptured) \iid -> do
        placeTokens attrs iid AlarmLevel 1
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> do
          alarmLevel <- getAlarmLevel iid
          skillValue <- getSkillTestModifiedSkillValue
          when (alarmLevel > skillValue) do
            afterSkillTest $ drawEncounterCard iid Cultist
        Tablet -> do
          placeTokens TabletEffect iid AlarmLevel 1
        _ -> pure ()
      pure s
    PassedSkillTest _ _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        ElderThing -> void $ runMaybeT do
          Action.Evade <- MaybeT getSkillTestAction
          EnemyTarget eid <- MaybeT getSkillTestTarget
          lift (assignEnemyDamage (nonAttack ElderThingEffect 2) eid)
        _ -> pure ()
      pure s
    ScenarioResolution r -> do
      case r of
        NoResolution -> do
          lead <- getLead
          story $ i18n "dreamEaters.darkSideOfTheMoon.noResolution"
          record TheInvestigatorsWereCarriedToTheColdWastes
          record RandolphCarterDidNotSurviveTheVoyage
          removeCampaignCard Assets.randolphCarterExpertDreamer
          forceAddCampaignCardToDeckChoice [lead] Treacheries.falseAwakening
          allGainXp attrs
          endOfScenario
        Resolution 1 -> do
          story $ i18n "dreamEaters.darkSideOfTheMoon.resolution1"
          record TheInvestigatorsTraveledToTheColdWastes
          record RandolphSurvivedTheVoyage
          allGainXp attrs
          push $ IncrementRecordCount EvidenceOfKadath 3
          endOfScenario
        other -> throw $ UnknownResolution other
      pure s
    _ -> DarkSideOfTheMoon <$> liftRunMessage msg attrs
