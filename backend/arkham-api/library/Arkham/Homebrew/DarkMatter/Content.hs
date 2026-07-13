module Arkham.Homebrew.DarkMatter.Content where

import Arkham.Act.Types (SomeActCard (..))
import Arkham.Agenda.Types (SomeAgendaCard (..))
import Arkham.Asset.Types (SomeAssetCard (..))
import Arkham.Card.CardCode
import Arkham.EncounterSet (EncounterSet)
import Arkham.Enemy.Types (SomeEnemyCard (..))
import Arkham.Homebrew.DarkMatter.Campaign (darkMatter)
import Arkham.Homebrew.DarkMatter.Sets qualified as Sets
import Arkham.Homebrew.Types
import Arkham.Id (CampaignId)
import Arkham.Location.Types (SomeLocationCard (..))
import Arkham.Treachery.Types (SomeTreacheryCard (..))
import Arkham.Homebrew.DarkMatter.Assets.K2PS187100Functionality
import Arkham.Homebrew.DarkMatter.Assets.K2PS18725Functionality
import Arkham.Homebrew.DarkMatter.Assets.K2PS18750Functionality
import Arkham.Homebrew.DarkMatter.Assets.K2PS18775Functionality
import Arkham.Homebrew.DarkMatter.Assets.Maja
import Arkham.Homebrew.DarkMatter.Enemies.StalkingByakhee
import Arkham.Homebrew.DarkMatter.Enemies.SystemBug
import Arkham.Homebrew.DarkMatter.Enemies.TheBOOGEYMAN
import Arkham.Homebrew.DarkMatter.Enemies.TheFeasterFromAfar
import Arkham.Homebrew.DarkMatter.Enemies.ViciousByakhee
import Arkham.Homebrew.DarkMatter.Treacheries.Anachronism
import Arkham.Homebrew.DarkMatter.Treacheries.ColdVacuum
import Arkham.Homebrew.DarkMatter.Treacheries.FutureEvils
import Arkham.Homebrew.DarkMatter.Treacheries.GrimFuture
import Arkham.Homebrew.DarkMatter.Treacheries.HauntingPast
import Arkham.Homebrew.DarkMatter.Treacheries.Micrometeoroid
import Arkham.Homebrew.DarkMatter.Treacheries.RadiantCrown
import Arkham.Homebrew.DarkMatter.Treacheries.SolarFlare
import Arkham.Homebrew.DarkMatter.Scenarios.ElectricNightmare
import Arkham.Homebrew.DarkMatter.Scenarios.FragmentOfCarcosa
import Arkham.Homebrew.DarkMatter.Scenarios.InTheShadowOfEarth
import Arkham.Homebrew.DarkMatter.Scenarios.LostQuantum
import Arkham.Homebrew.DarkMatter.Scenarios.Starfall
import Arkham.Homebrew.DarkMatter.Scenarios.StrangeMoons
import Arkham.Homebrew.DarkMatter.Scenarios.TheMachineInYellow
import Arkham.Homebrew.DarkMatter.Scenarios.TheTatterdemalion

acts :: [SomeActCard]
acts = []

agendas :: [SomeAgendaCard]
agendas = []

assets :: [SomeAssetCard]
assets =
  [ SomeAssetCard k2PS187100Functionality
  , SomeAssetCard k2PS18725Functionality
  , SomeAssetCard k2PS18750Functionality
  , SomeAssetCard k2PS18775Functionality
  , SomeAssetCard maja
  ]

enemies :: [SomeEnemyCard]
enemies =
  [ SomeEnemyCard stalkingByakhee
  , SomeEnemyCard systemBug
  , SomeEnemyCard theBOOGEYMAN
  , SomeEnemyCard theFeasterFromAfar
  , SomeEnemyCard viciousByakhee
  ]

locations :: [SomeLocationCard]
locations = []

treacheries :: [SomeTreacheryCard]
treacheries =
  [ SomeTreacheryCard anachronism
  , SomeTreacheryCard coldVacuum
  , SomeTreacheryCard futureEvils
  , SomeTreacheryCard grimFuture
  , SomeTreacheryCard hauntingPast
  , SomeTreacheryCard micrometeoroid
  , SomeTreacheryCard radiantCrown
  , SomeTreacheryCard solarFlare
  ]

scenarios :: [(CardCode, HomebrewScenario)]
scenarios =
  [ ("z-dark-matter-013", HomebrewScenario theTatterdemalion)
  , ("z-dark-matter-053", HomebrewScenario electricNightmare)
  , ("z-dark-matter-090", HomebrewScenario lostQuantum)
  , ("z-dark-matter-115", HomebrewScenario inTheShadowOfEarth)
  , ("z-dark-matter-156", HomebrewScenario strangeMoons)
  , ("z-dark-matter-193", HomebrewScenario theMachineInYellow)
  , ("z-dark-matter-212", HomebrewScenario fragmentOfCarcosa)
  , ("z-dark-matter-246", HomebrewScenario starfall)
  ]

scenarioSets :: [(CardCode, EncounterSet)]
scenarioSets =
  [ ("z-dark-matter-013", Sets.TheTatterdemalion)
  , ("z-dark-matter-053", Sets.ElectricNightmare)
  , ("z-dark-matter-090", Sets.LostQuantum)
  , ("z-dark-matter-115", Sets.InTheShadowOfEarth)
  , ("z-dark-matter-156", Sets.StrangeMoons)
  , ("z-dark-matter-193", Sets.TheMachineInYellow)
  , ("z-dark-matter-212", Sets.FragmentOfCarcosa)
  , ("z-dark-matter-246", Sets.Starfall)
  ]

campaigns :: [(CampaignId, HomebrewCampaign)]
campaigns = [("z-dark-matter", HomebrewCampaign darkMatter)]
