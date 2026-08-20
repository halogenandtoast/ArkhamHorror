module Arkham.Scenario.Scenarios.TheDreamEaters.DarkSideOfTheMoon (darkSideOfTheMoon) where

import Arkham.Act.CardDefs.TheDreamEaters.DarkSideOfTheMoon qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.CardDefs.TheDreamEaters.DarkSideOfTheMoon qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.Card
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TheDreamEaters.DarkSideOfTheMoon qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Campaign (getCampaignStoryCard)
import Arkham.Helpers.FlavorText (additionalRules, li, setup, ul)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.I18n
import Arkham.Location.CardDefs.TheDreamEaters.DarkSideOfTheMoon qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted hiding (assignEnemyDamage, drawEncounterCard, story)
import Arkham.Scenarios.TheDreamEaters.DarkSideOfTheMoon.Helpers
import Arkham.Token
import Arkham.Treachery.CardDefs.TheDreamEaters.DarkSideOfTheMoon qualified as Treacheries

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
      pure $ toChaosTokenValue attrs Skull ((alarmLevel + 1) `div` 2) alarmLevel
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
  runMessage msg s@(DarkSideOfTheMoon attrs) = runQueueT $ withI18n $ case msg of
    StandaloneSetup -> do
      push $ SetChaosTokens standaloneChaosTokens
      pure s
    PreScenarioSetup -> do
      whenHasRecord RandolphWasCaptured do
        getCampaignStoryCard Assets.randolphCarterExpertDreamer >>= push . SetAsideCards . pure . toCard

      story $ i18nWithTitle "theDreamEaters.darkSideOfTheMoon.intro"
      captured <- selectAny $ investigatorWithRecord WasCaptured
      story
        $ i18nWithTitle
        $ if captured
          then "theDreamEaters.darkSideOfTheMoon.intro1"
          else "theDreamEaters.darkSideOfTheMoon.intro2"
      pure s
    Setup -> runScenarioSetup DarkSideOfTheMoon attrs do
      scenarioI18n $ setup do
        ul do
          li "gatherSets"
          li.nested "putLocations" $ li "setOtherLocationsAside"
          li.nested "checkCaptured" do
            li "putMoonBeastGalley"
            li "capturedInvestigatorsBegin"
            li "otherInvestigatorsBegin"
          li.nested "checkRandolph" $ li "setRandolphAside"
          li "setCardsAside"
          li "placeAlarmLevel"
          li "buildEncounterDeck"

      scenarioI18n $ additionalRules "alarmLevel"

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

      captured <- select $ investigatorWithRecord WasCaptured
      if notNull captured
        then do
          moonBeastGalley <- place Locations.moonBeastGalley
          for_ captured \iid -> do
            moveTo_ attrs iid moonBeastGalley
            placeClues attrs moonBeastGalley 2
        else setAside [Locations.moonBeastGalley]

      notCaptured <- select $ not_ (investigatorWithRecord WasCaptured)
      for_ notCaptured \iid -> moveTo_ attrs iid moonForest

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
          skillValue <- getModifiedSkillValue
          when (alarmLevel > skillValue) do
            afterSkillTestQuiet $ drawEncounterCard iid Cultist
        Tablet -> raiseAlarmLevel Tablet [iid]
        _ -> pure ()
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        ElderThing -> void $ runMaybeT do
          Action.Evade <- MaybeT getSkillTestAction
          EnemyTarget eid <- MaybeT getSkillTestTarget
          lift (assignEnemyDamage (nonAttack (Just iid) ElderThingEffect 2) eid)
        _ -> pure ()
      pure s
    ScenarioResolution r -> scenarioI18n $ scope "resolutions" do
      case r of
        NoResolution -> do
          lead <- getLead
          resolutionWithXp "noResolution" $ allGainXp' attrs
          record TheInvestigatorsWereCarriedToTheColdWastes
          record RandolphCarterDidNotSurviveTheVoyage
          removeCampaignCard Assets.randolphCarterExpertDreamer
          forceAddCampaignCardToDeckChoice [lead] DoNotShuffleIn Treacheries.falseAwakening
          endOfScenario
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          record TheInvestigatorsTraveledToTheColdWastes
          record RandolphSurvivedTheVoyage
          incrementRecordCount EvidenceOfKadath 3
          endOfScenario
        other -> throw $ UnknownResolution other
      pure s
    _ -> DarkSideOfTheMoon <$> liftRunMessage msg attrs
