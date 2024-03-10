module Arkham.Scenario.Scenarios.DarkSideOfTheMoon (
  DarkSideOfTheMoon (..),
  darkSideOfTheMoon,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign (getCampaignStoryCard)
import Arkham.Helpers.Log (whenHasRecord)
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Scenario.Runner hiding (story)
import Arkham.Scenario.Setup
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
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DarkSideOfTheMoon where
  runMessage msg s@(DarkSideOfTheMoon attrs) = runQueueT $ case msg of
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

      setAgendaDeck [Agendas.silentStiring, Agendas.theAlarmIsRaised, Agendas.theyAreUponYou]
      setActDeck
        [Acts.inTheBellyOfTheMoonBeast, Acts.exploringTheMoon, Acts.theMoonsCore, Acts.unexpectedRescue]

      place_ Locations.cityOfTheMoonBeasts
      place_ Locations.templeOfTheMoonLizard
      moonForest <- place Locations.moonForest
      place_ Locations.theDarkCrater

      captured <- select $ InvestigatorWithRecord WasCaptured
      if (notNull captured)
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
        push $ PlaceTokens (toSource attrs) (toTarget iid) AlarmLevel 1
    _ -> DarkSideOfTheMoon <$> lift (runMessage msg attrs)
