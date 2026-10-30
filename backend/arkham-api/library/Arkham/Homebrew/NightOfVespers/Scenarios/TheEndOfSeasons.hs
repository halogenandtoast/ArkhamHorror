-- Chaos token effects and resolutions are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Scenarios.TheEndOfSeasons (theEndOfSeasons) where

import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.NightOfVespers.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.NightOfVespers.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.NightOfVespers.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Homebrew.NightOfVespers.Sets qualified as Set
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (StartingClues))
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Criminal, Detective, Police, Reporter))

newtype TheEndOfSeasons = TheEndOfSeasons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

theEndOfSeasons :: Difficulty -> TheEndOfSeasons
theEndOfSeasons difficulty =
  scenario
    TheEndOfSeasons
    ":night-of-vespers:001"
    "The End of Seasons"
    difficulty
    [ ".        .        .        plus    ."
    , ".        square   hourglass heart   ."
    , "circle   diamond  star     trefoil t"
    , "triangle moon     droplet  .       equals"
    , ".        squiggle .        .       ."
    ]

instance RunMessage TheEndOfSeasons where
  runMessage msg s@(TheEndOfSeasons attrs) =
    runQueueT $ scenarioI18n "theEndOfSeasons" $ runJinx msg *> case msg of
      PreScenarioSetup -> scope "intro" do
        readers <- select $ mapOneOf InvestigatorWithTrait [Criminal, Detective, Police, Reporter]
        flavor do
          h "title"
          p "part1"
          compose.green.validate (notNull readers) do
            ul do
              li "underworld"
              li "bonus"
          p "proceed"

        n <- getPlayerCount
        case readers of
          [] -> pure ()
          (iid : _) | n == 1 -> setupModifier ScenarioSource iid $ StartingClues n
          _ -> leadChooseOneM do
            questionLabeled "underworld"
            targets readers \iid -> setupModifier ScenarioSource iid $ StartingClues n

        flavor $ h "title" >> p "part2"
        pure s
      Setup -> runScenarioSetup TheEndOfSeasons attrs do
        setup $ ul do
          li "gatherSets"
          li "inferno"
          li "placeLocations"
          li "setLocationsAside"
          li "removeFromPlay"
          li "declan"
          li "decks"
          unscoped $ li "shuffleRemainder"
          unscoped $ li "readyToBegin"

        additionalRules "locations"

        gather Set.TheEndOfSeasons
        gather Set.TheFamily
        gather Set.HostileArchitecture
        gather Set.LiminalSpaces
        gatherAndSetAside Set.Inferno

        setActDeck [Acts.thusWithAKiss, Acts.aFamilyAffair, Acts.intoTheFire]
        setAgendaDeck [Agendas.tooSwiftArrives, Agendas.openSeason, Agendas.callingItANight]

        removeEvery
          =<< traverse
            sampleOneOf
            [ (Locations.barLAmmaliatrice_013, Locations.barLAmmaliatrice_014)
            , (Locations.galerieLavandiere_015, Locations.galerieLavandiere_016)
            , (Locations.trattoriaKitchens_017, Locations.trattoriaKitchens_018)
            , (Locations.hotelLobby_019, Locations.hotelLobby_020)
            , (Locations.luxurySuites_021, Locations.luxurySuites_022)
            , (Locations.mezzanineElevators_023, Locations.mezzanineElevators_024)
            , (Locations.pleasureGardens_025, Locations.pleasureGardens_026)
            , (Locations.presidentialSuite_027, Locations.presidentialSuite_028)
            , (Locations.hotelWineCellar_029, Locations.hotelWineCellar_030)
            ]
        startAt =<< place Locations.trattoriaDelParcheLaDolceVita
        setAsideEvery #location
        setAside [Enemies.declanODoyle]
      _ -> TheEndOfSeasons <$> liftRunMessage msg attrs
