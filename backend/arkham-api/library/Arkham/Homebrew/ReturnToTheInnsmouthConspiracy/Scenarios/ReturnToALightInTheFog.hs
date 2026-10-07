module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToALightInTheFog (returnToALightInTheFog) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Acts
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Agendas qualified as HBAgendas
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Enemies qualified as HBEnemies
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (
  combineTidalTunnels,
  deepOneInvestigator,
  officialSetup,
  scenarioI18n,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Key
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Locations
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.ALightInTheFog
import Arkham.Story.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Stories
import Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Treacheries

newtype ReturnToALightInTheFog = ReturnToALightInTheFog ALightInTheFog
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToALightInTheFog :: Difficulty -> ReturnToALightInTheFog
returnToALightInTheFog difficulty =
  scenarioWith
    (ReturnToALightInTheFog . ALightInTheFog)
    ":return-to-the-innsmouth-conspiracy:039"
    "Return to ALightInTheFog"
    difficulty
    []
    (referenceL .~ "07231")

instance RunMessage ReturnToALightInTheFog where
  runMessage msg (ReturnToALightInTheFog inner@(ALightInTheFog attrs)) =
    runQueueT $ scenarioI18n "returnToALightInTheFog" $ case msg of
      Setup -> runScenarioSetup (ReturnToALightInTheFog . ALightInTheFog) attrs do
        setIsReturnTo
        replaceSet Set.Syzygy Sets.Occultation
        replaceSet Set.RisingTide Sets.RollingTide
        substitute Agendas.terrorAtFalconPoint HBAgendas.terrorAtFalconPointV2
        idolBrought <- getHasRecord TheIdolWasBroughtToTheLighthouse
        mantleBrought <- getHasRecord TheMantleWasBroughtToTheLighthouse
        headdressBrought <- getHasRecord TheHeaddressWasBroughtToTheLighthouse
        afterSunrise <- getHasRecord TheInvestigatorsReachedFalconPointAfterSunrise
        tideGrownStronger <- getHasRecord TheTideHasGrownStronger

        setup $ ul do
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
            li.returnTo "replacedCards"
          li.returnTo "tidalTunnels"
          officialSetup "aLightInTheFog" do
            li.nested "placeLocations" do
              li "startAt"
              li "removeUndergroundRivers"
              li "setAsideOtherLocations"
            li.nested "placeKeys" do
              li "faceupKeys"
              li "facedownKeys"
            li "captured"
            li "setAsideCards"
          li.returnTo "setAsideGrapplers"
          officialSetup "aLightInTheFog" do
            li.nested "checkCampaignLog" do
              li.validate idolBrought "wavewornIdol"
              li.validate mantleBrought "awakenedMantle"
              li.validate headdressBrought "headdressOfYhaNthlei"
            li.nested "checkCampaignLogDoom" do
              li.validate afterSunrise "afterSunrise"
              li.validate tideGrownStronger "tideHasGrownStronger"
            li "floodTokens"
          unscoped $ li "shuffleRemainder"

        gather Sets.ReturnToALightInTheFog
        gather Sets.ReturnToFloodedCaverns
        gather Set.ALightInTheFog
        gather Set.CreaturesOfTheDeep
        gather Set.FloodedCaverns
        gather Set.RisingTide
        gather Set.Syzygy
        gather Set.StrikingFear

        setAgendaDeck
          [ Agendas.fogOnTheBay
          , Agendas.unchangingAsTheSea
          , Agendas.theTideRises
          , Agendas.terrorAtFalconPoint
          ]
        setActDeck [Acts.theLighthouse, Acts.findingThePath, Acts.worshippersOfTheDeep]

        falconPointGatehouse <- placeInGrid (Pos 0 0) Locations.falconPointGatehouse
        placeInGrid_ (Pos 1 0) Locations.falconPointCliffside
        placeInGrid_ (Pos 2 0) Locations.lighthouseStairwell
        placeInGrid_ (Pos 3 0) Locations.lighthouseKeepersCottage
        placeInGrid_ (Pos 2 1) Locations.lanternRoom

        startAt falconPointGatehouse

        setAside
          . combineTidalTunnels
          =<< amongGathered
            ( CardWithTitle "Tidal Tunnel"
                <> not_ (mapOneOf cardIs [Locations.undergroundRiver, HBLocations.undergroundRiver])
            )
        -- "Set aside all three copies of Deep One Grappler, out of play."
        setAside $ replicate 3 HBEnemies.deepOneGrappler

        randomizedKeys <- shuffle $ map UnrevealedKey [PurpleKey, GreenKey]
        setAsideKeys $ [WhiteKey, BlackKey, BlueKey, YellowKey, RedKey] <> randomizedKeys

        placeStory Stories.captured

        setAside
          [ Enemies.oceirosMarsh
          , Treacheries.worthHisSalt
          , Treacheries.worthHisSalt
          , Treacheries.takenCaptive
          , Treacheries.takenCaptive
          , Locations.sunkenGrottoUpperDepths
          , Locations.sunkenGrottoLowerDepths
          , Locations.sunkenGrottoFinalDepths
          ]

        whenHasRecord TheIdolWasBroughtToTheLighthouse $ setAside [Assets.wavewornIdol]
        whenHasRecord TheMantleWasBroughtToTheLighthouse $ setAside [Assets.awakenedMantle]
        whenHasRecord TheHeaddressWasBroughtToTheLighthouse $ setAside [Assets.headdressOfYhaNthlei]
        whenHasRecord TheInvestigatorsReachedFalconPointAfterSunrise $ placeDoomOnAgenda 1
        whenHasRecord TheTideHasGrownStronger $ placeDoomOnAgenda 1
      {- "While resolving one of Resolution 1, 2 or 4, every Deep One investigator has to
      read this: ... You gain 1 mental trauma as your mind tries to cope with the changes
      your body is going through." -}
      ScenarioResolution r | r `elem` map Resolution [1, 2, 4] -> do
        deepOnes <- select deepOneInvestigator
        unless (null deepOnes) do
          story $ i18nWithTitle "changing"
          for_ deepOnes (`sufferMentalTrauma` 1)
        ReturnToALightInTheFog <$> liftRunMessage msg inner
      _ -> ReturnToALightInTheFog <$> liftRunMessage msg inner
