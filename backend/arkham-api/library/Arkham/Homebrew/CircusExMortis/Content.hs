module Arkham.Homebrew.CircusExMortis.Content where

import Arkham.Act.Types (SomeActCard (..))
import Arkham.Agenda.Types (SomeAgendaCard (..))
import Arkham.Asset.Types (SomeAssetCard (..))
import Arkham.Enemy.Types (SomeEnemyCard (..))
import Arkham.Homebrew.CircusExMortis.Acts.ForestOfIllusion
import Arkham.Homebrew.CircusExMortis.Acts.OutAndAway
import Arkham.Homebrew.CircusExMortis.Acts.RatsInACage
import Arkham.Homebrew.CircusExMortis.Acts.SmokeAndMirrors
import Arkham.Homebrew.CircusExMortis.Agendas.BloodMoon
import Arkham.Homebrew.CircusExMortis.Agendas.HouseOfHorrors
import Arkham.Homebrew.CircusExMortis.Agendas.MesmericMagic
import Arkham.Homebrew.CircusExMortis.Agendas.SavageNature
import Arkham.Homebrew.CircusExMortis.Agendas.TheTrueFace
import Arkham.Homebrew.CircusExMortis.Assets.IllusoryLocus
import Arkham.Homebrew.CircusExMortis.Campaign (circusExMortis)
import Arkham.Homebrew.CircusExMortis.Enemies.CircusPredator
import Arkham.Homebrew.CircusExMortis.Enemies.DisguisedMonstrosity
import Arkham.Homebrew.CircusExMortis.Enemies.GrotesqueLion
import Arkham.Homebrew.CircusExMortis.Enemies.Mooncalf
import Arkham.Homebrew.CircusExMortis.Enemies.NascentDarkYoung
import Arkham.Homebrew.CircusExMortis.Enemies.NewMoonAcrobat
import Arkham.Homebrew.CircusExMortis.Enemies.NewMoonBeastTamer
import Arkham.Homebrew.CircusExMortis.Enemies.NewMoonCarny
import Arkham.Homebrew.CircusExMortis.Enemies.NewMoonClown
import Arkham.Homebrew.CircusExMortis.Enemies.NewMoonDrudge
import Arkham.Homebrew.CircusExMortis.Enemies.NewMoonIllusionist
import Arkham.Homebrew.CircusExMortis.Enemies.NewMoonMagician
import Arkham.Homebrew.CircusExMortis.Enemies.NewMoonStrongman
import Arkham.Homebrew.CircusExMortis.Enemies.SupplicantOfTheGoat
import Arkham.Homebrew.CircusExMortis.Enemies.TwistedSatyr
import Arkham.Homebrew.CircusExMortis.Enemies.UrsineBrute
import Arkham.Homebrew.CircusExMortis.Locations.AnimalCages
import Arkham.Homebrew.CircusExMortis.Locations.Carousel
import Arkham.Homebrew.CircusExMortis.Locations.CircusEncampment
import Arkham.Homebrew.CircusExMortis.Locations.CircusGatesPathToFreedom
import Arkham.Homebrew.CircusExMortis.Locations.ForestPassage
import Arkham.Homebrew.CircusExMortis.Locations.GamesGallery
import Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestCircularGrove
import Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestDeadGrove
import Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestFogBank
import Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestGlassyLake
import Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestLabyrinthOfTrees
import Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestMistyMarsh
import Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestQuietValley
import Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestShadowedPath
import Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestShallowRiver
import Arkham.Homebrew.CircusExMortis.Locations.PerformerTrailers
import Arkham.Homebrew.CircusExMortis.Locations.RemoteCabin
import Arkham.Homebrew.CircusExMortis.Locations.TheBigTopFirstRing
import Arkham.Homebrew.CircusExMortis.Locations.TheBigTopSecondRing
import Arkham.Homebrew.CircusExMortis.Locations.TheBigTopThirdRing
import Arkham.Homebrew.CircusExMortis.Locations.WoodlandOverlook
import Arkham.Homebrew.CircusExMortis.Scenarios.AllPointsWest
import Arkham.Homebrew.CircusExMortis.Scenarios.Bacchanalia
import Arkham.Homebrew.CircusExMortis.Scenarios.HarmsWay
import Arkham.Homebrew.CircusExMortis.Scenarios.OneNightOnly
import Arkham.Homebrew.CircusExMortis.Scenarios.PiperAtTheGatesOfDawn
import Arkham.Homebrew.CircusExMortis.Scenarios.RedSunrise
import Arkham.Homebrew.CircusExMortis.Scenarios.ThePrimrosePath
import Arkham.Homebrew.CircusExMortis.Scenarios.ThousandToOne
import Arkham.Homebrew.CircusExMortis.Sets qualified as Sets
import Arkham.Homebrew.CircusExMortis.Treacheries.EndlessSpawn
import Arkham.Homebrew.CircusExMortis.Treacheries.FeralImpulses
import Arkham.Homebrew.CircusExMortis.Treacheries.LunarInfluence
import Arkham.Homebrew.CircusExMortis.Treacheries.MaddeningSpectacle
import Arkham.Homebrew.CircusExMortis.Treacheries.MilkOfShubNiggurath
import Arkham.Homebrew.CircusExMortis.Treacheries.MoonlightIllusion
import Arkham.Homebrew.CircusExMortis.Treacheries.OminousMoonlight
import Arkham.Homebrew.CircusExMortis.Treacheries.QuickerThanTheEye
import Arkham.Homebrew.CircusExMortis.Treacheries.RecklessStunt
import Arkham.Homebrew.Types
import Arkham.Location.Types (SomeLocationCard (..))
import Arkham.Treachery.Types (SomeTreacheryCard (..))

acts :: [SomeActCard]
acts =
  [ SomeActCard forestOfIllusion
  , SomeActCard outAndAway
  , SomeActCard ratsInACage_005
  , SomeActCard ratsInACage_006
  , SomeActCard ratsInACage_007
  , SomeActCard ratsInACage_008
  , SomeActCard smokeAndMirrors
  ]

agendas :: [SomeAgendaCard]
agendas =
  [ SomeAgendaCard bloodMoon
  , SomeAgendaCard houseOfHorrors
  , SomeAgendaCard mesmericMagic
  , SomeAgendaCard savageNature
  , SomeAgendaCard theTrueFace
  ]

assets :: [SomeAssetCard]
assets =
  [ SomeAssetCard illusoryLocus
  ]

enemies :: [SomeEnemyCard]
enemies =
  [ SomeEnemyCard circusPredator
  , SomeEnemyCard disguisedMonstrosity
  , SomeEnemyCard grotesqueLion
  , SomeEnemyCard mooncalf
  , SomeEnemyCard nascentDarkYoung
  , SomeEnemyCard newMoonAcrobat
  , SomeEnemyCard newMoonBeastTamer
  , SomeEnemyCard newMoonCarny
  , SomeEnemyCard newMoonClown
  , SomeEnemyCard newMoonDrudge
  , SomeEnemyCard newMoonIllusionist
  , SomeEnemyCard newMoonMagician
  , SomeEnemyCard newMoonStrongman
  , SomeEnemyCard supplicantOfTheGoat
  , SomeEnemyCard twistedSatyr
  , SomeEnemyCard ursineBrute
  ]

locations :: [SomeLocationCard]
locations =
  [ SomeLocationCard animalCages
  , SomeLocationCard carousel
  , SomeLocationCard circusEncampment
  , SomeLocationCard circusGatesPathToFreedom
  , SomeLocationCard forestPassage
  , SomeLocationCard gamesGallery
  , SomeLocationCard moonlitForestCircularGrove
  , SomeLocationCard moonlitForestDeadGrove
  , SomeLocationCard moonlitForestFogBank
  , SomeLocationCard moonlitForestGlassyLake
  , SomeLocationCard moonlitForestLabyrinthOfTrees
  , SomeLocationCard moonlitForestMistyMarsh
  , SomeLocationCard moonlitForestQuietValley
  , SomeLocationCard moonlitForestShadowedPath
  , SomeLocationCard moonlitForestShallowRiver
  , SomeLocationCard performerTrailers
  , SomeLocationCard remoteCabin
  , SomeLocationCard theBigTopFirstRing
  , SomeLocationCard theBigTopSecondRing
  , SomeLocationCard theBigTopThirdRing
  , SomeLocationCard woodlandOverlook
  ]

treacheries :: [SomeTreacheryCard]
treacheries =
  [ SomeTreacheryCard endlessSpawn
  , SomeTreacheryCard feralImpulses
  , SomeTreacheryCard lunarInfluence
  , SomeTreacheryCard maddeningSpectacle
  , SomeTreacheryCard milkOfShubNiggurath
  , SomeTreacheryCard moonlightIllusion
  , SomeTreacheryCard ominousMoonlight
  , SomeTreacheryCard quickerThanTheEye
  , SomeTreacheryCard recklessStunt
  ]

scenarios :: HomebrewScenarios
scenarios =
  [ (":circus-ex-mortis:001", HomebrewScenario Sets.OneNightOnly oneNightOnly)
  , (":circus-ex-mortis:017", HomebrewScenario Sets.ThePrimrosePath thePrimrosePath)
  , (":circus-ex-mortis:042", HomebrewScenario Sets.HarmsWay harmsWay)
  , (":circus-ex-mortis:076", HomebrewScenario Sets.AllPointsWest allPointsWest)
  , (":circus-ex-mortis:110", HomebrewScenario Sets.PiperAtTheGatesOfDawn piperAtTheGatesOfDawn)
  , (":circus-ex-mortis:124", HomebrewScenario Sets.Bacchanalia bacchanalia)
  , (":circus-ex-mortis:155", HomebrewScenario Sets.RedSunrise redSunrise)
  , (":circus-ex-mortis:192", HomebrewScenario Sets.ThousandToOne thousandToOne)
  ]

campaigns :: HomebrewCampaigns
campaigns = [(":circus-ex-mortis", HomebrewCampaign circusExMortis)]

data CircusExMortisContent

instance IsHomebrewContent CircusExMortisContent where
  homebrewContent =
    HomebrewContent
      { acts = acts
      , agendas = agendas
      , assets = assets
      , enemies = enemies
      , locations = locations
      , stories = []
      , treacheries = treacheries
      , scenarios = scenarios
      , campaigns = campaigns
      }
