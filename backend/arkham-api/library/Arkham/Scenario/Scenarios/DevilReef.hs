module Arkham.Scenario.Scenarios.DevilReef (DevilReef (..), devilReef) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.EncounterSet qualified as Set
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DevilReef.Helpers

newtype DevilReef = DevilReef ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilReef :: Difficulty -> DevilReef
devilReef difficulty = scenario DevilReef "07163" "Devil Reef" difficulty []

instance HasChaosTokenValue DevilReef where
  getChaosTokenValue iid tokenFace (DevilReef attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DevilReef where
  runMessage msg s@(DevilReef attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      missionWasSuccessful <- getHasRecord TheMissionWasSuccessful
      doStep (if missionWasSuccessful then 2 else 3) msg
      pure s
    DoStep 2 PreScenarioSetup -> do
      story $ i18nWithTitle "intro2"
      pure s
    DoStep 3 PreScenarioSetup -> do
      story $ i18nWithTitle "intro3"
      pure s
    Setup -> runScenarioSetup DevilReef attrs do
      gather Set.DevilReef
      gather Set.AgentsOfHydra
      gather Set.CreaturesOfTheDeep
      gather Set.FloodedCaverns
      gather Set.Malfunction
      gather Set.RisingTide

      aBattle <- hasMemory ABattleWithAHorrifyingDevil

      let agenda1 = if aBattle then Agendas.secretsOfTheSeaV1 else Agendas.secretsOfTheSeaV2

      setAgendaDeck [agenda1, Agendas.theDevilOfTheDepths]
      setActDeck [Acts.reefOfMysteries]

      setAsideKeys [PurpleKey, WhiteKey, BlackKey]
      setAsideKeys . map UnrevealedKey =<< shuffleM [YellowKey, GreenKey, RedKey, BlueKey]

      churningWaters <- placeInGrid (Pos 0 0) Locations.churningWaters
      push $ SetFloodLevel churningWaters FullyFlooded
      fishingVessel <- assetAt Assets.fishingVessel churningWaters
      eachInvestigator $ \iid -> push $ PlaceInvestigator iid (InVehicle fishingVessel)
      reveal churningWaters

      setAside [Assets.wavewornIdol, Assets.awakenedMantle, Assets.headdressOfYhaNthlei]

      cyclopeanRuins <- pickFrom (Locations.cyclopeanRuins_176a, Locations.cyclopeanRuins_176b)
      deepOneGrotto <- pickFrom (Locations.deepOneGrotto_175a, Locations.deepOneGrotto_175b)
      templeOfTheUnion <- pickFrom (Locations.templeOfTheUnion_177a, Locations.templeOfTheUnion_177b)

      setAside [cyclopeanRuins, deepOneGrotto, templeOfTheUnion]

      zipWithM_ placeInGrid [Pos 0 3, Pos 4 2, Pos (-4) 2, Pos 4 (-2), Pos (-4) (-2)]
        =<< shuffleM
          [ Locations.lonelyIsle
          , Locations.hiddenCove
          , Locations.wavewornIsland
          , Locations.saltMarshes
          , Locations.blackReef
          ]
      addExtraDeck TidalTunnelDeck
        =<< shuffleM
          [ Locations.boneRiddenPit
          , Locations.fishGraveyard
          , Locations.underwaterCavern
          , Locations.underwaterCavern
          , Locations.tidalPool
          , Locations.tidalPool
          , Locations.undergroundRiver
          , Locations.undergroundRiver
          ]
    _ -> DevilReef <$> liftRunMessage msg attrs
