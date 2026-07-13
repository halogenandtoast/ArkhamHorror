module Arkham.Homebrew.CircusExMortis.Content where

import Arkham.Act.Types (SomeActCard (..))
import Arkham.Agenda.Types (SomeAgendaCard (..))
import Arkham.Asset.Types (SomeAssetCard (..))
import Arkham.Card.CardCode
import Arkham.EncounterSet (EncounterSet)
import Arkham.Enemy.Types (SomeEnemyCard (..))
import Arkham.Homebrew.CircusExMortis.Campaign (circusExMortis)
import Arkham.Homebrew.CircusExMortis.Sets qualified as Sets
import Arkham.Homebrew.Types
import Arkham.Id (CampaignId)
import Arkham.Location.Types (SomeLocationCard (..))
import Arkham.Treachery.Types (SomeTreacheryCard (..))
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
import Arkham.Homebrew.CircusExMortis.Treacheries.EndlessSpawn
import Arkham.Homebrew.CircusExMortis.Treacheries.FeralImpulses
import Arkham.Homebrew.CircusExMortis.Treacheries.LunarInfluence
import Arkham.Homebrew.CircusExMortis.Treacheries.MaddeningSpectacle
import Arkham.Homebrew.CircusExMortis.Treacheries.MilkOfShubNiggurath
import Arkham.Homebrew.CircusExMortis.Treacheries.MoonlightIllusion
import Arkham.Homebrew.CircusExMortis.Treacheries.OminousMoonlight
import Arkham.Homebrew.CircusExMortis.Treacheries.QuickerThanTheEye
import Arkham.Homebrew.CircusExMortis.Treacheries.RecklessStunt
import Arkham.Homebrew.CircusExMortis.Scenarios.AllPointsWest
import Arkham.Homebrew.CircusExMortis.Scenarios.Bacchanalia
import Arkham.Homebrew.CircusExMortis.Scenarios.HarmsWay
import Arkham.Homebrew.CircusExMortis.Scenarios.OneNightOnly
import Arkham.Homebrew.CircusExMortis.Scenarios.PiperAtTheGatesOfDawn
import Arkham.Homebrew.CircusExMortis.Scenarios.RedSunrise
import Arkham.Homebrew.CircusExMortis.Scenarios.ThePrimrosePath
import Arkham.Homebrew.CircusExMortis.Scenarios.ThousandToOne

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

scenarios :: [(CardCode, HomebrewScenario)]
scenarios =
  [ ("z-circus-ex-mortis-001", HomebrewScenario oneNightOnly)
  , ("z-circus-ex-mortis-017", HomebrewScenario thePrimrosePath)
  , ("z-circus-ex-mortis-042", HomebrewScenario harmsWay)
  , ("z-circus-ex-mortis-076", HomebrewScenario allPointsWest)
  , ("z-circus-ex-mortis-110", HomebrewScenario piperAtTheGatesOfDawn)
  , ("z-circus-ex-mortis-124", HomebrewScenario bacchanalia)
  , ("z-circus-ex-mortis-155", HomebrewScenario redSunrise)
  , ("z-circus-ex-mortis-192", HomebrewScenario thousandToOne)
  ]

scenarioSets :: [(CardCode, EncounterSet)]
scenarioSets =
  [ ("z-circus-ex-mortis-001", Sets.OneNightOnly)
  , ("z-circus-ex-mortis-017", Sets.ThePrimrosePath)
  , ("z-circus-ex-mortis-042", Sets.HarmsWay)
  , ("z-circus-ex-mortis-076", Sets.AllPointsWest)
  , ("z-circus-ex-mortis-110", Sets.PiperAtTheGatesOfDawn)
  , ("z-circus-ex-mortis-124", Sets.Bacchanalia)
  , ("z-circus-ex-mortis-155", Sets.RedSunrise)
  , ("z-circus-ex-mortis-192", Sets.ThousandToOne)
  ]

campaigns :: [(CampaignId, HomebrewCampaign)]
campaigns = [("z-circus-ex-mortis", HomebrewCampaign circusExMortis)]
