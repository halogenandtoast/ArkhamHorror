module Arkham.Scenario.Scenarios.WeaverOfTheCosmos (
  WeaverOfTheCosmos (..),
  weaverOfTheCosmos,
) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Difficulty
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log (getRecordCount)
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted
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
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WeaverOfTheCosmos where
  runMessage msg s@(WeaverOfTheCosmos attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "dreamEaters.weaverOfTheCosmos.intro"
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
    _ -> WeaverOfTheCosmos <$> lift (runMessage msg attrs)
