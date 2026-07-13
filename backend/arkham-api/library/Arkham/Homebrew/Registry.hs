module Arkham.Homebrew.Registry where

import Arkham.Act.Types (SomeActCard)
import Arkham.Agenda.Types (SomeAgendaCard)
import Arkham.Asset.Types (SomeAssetCard)
import Arkham.Card.CardCode
import Arkham.EncounterSet (EncounterSet)
import Arkham.Enemy.Types (SomeEnemyCard)
import Arkham.Homebrew.Types as X
import Arkham.Id (CampaignId)
import Arkham.Location.Types (SomeLocationCard)
import Arkham.Prelude
import Arkham.Treachery.Types (SomeTreacheryCard)
import Arkham.Homebrew.DarkMatter.Content qualified as DarkMatter
import Arkham.Homebrew.CircusExMortis.Content qualified as CircusExMortis

acts :: [SomeActCard]
acts = DarkMatter.acts <> CircusExMortis.acts

agendas :: [SomeAgendaCard]
agendas = DarkMatter.agendas <> CircusExMortis.agendas

assets :: [SomeAssetCard]
assets = DarkMatter.assets <> CircusExMortis.assets

enemies :: [SomeEnemyCard]
enemies = DarkMatter.enemies <> CircusExMortis.enemies

locations :: [SomeLocationCard]
locations = DarkMatter.locations <> CircusExMortis.locations

treacheries :: [SomeTreacheryCard]
treacheries = DarkMatter.treacheries <> CircusExMortis.treacheries

scenarios :: [(CardCode, HomebrewScenario)]
scenarios = DarkMatter.scenarios <> CircusExMortis.scenarios

scenarioSets :: [(CardCode, EncounterSet)]
scenarioSets = DarkMatter.scenarioSets <> CircusExMortis.scenarioSets

campaigns :: [(CampaignId, HomebrewCampaign)]
campaigns = DarkMatter.campaigns <> CircusExMortis.campaigns
