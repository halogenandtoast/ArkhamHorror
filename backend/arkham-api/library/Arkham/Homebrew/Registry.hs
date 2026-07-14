{-# LANGUAGE TemplateHaskell #-}

{- | Runtime content contributed by homebrew campaigns. Campaigns are
discovered: any @Arkham/Homebrew/<Name>/Content.hs@ with an
'IsHomebrewContent' instance is folded in automatically — no edits here when
adding a campaign.
-}
module Arkham.Homebrew.Registry where

import Arkham.Act.Types (SomeActCard)
import Arkham.Agenda.Types (SomeAgendaCard)
import Arkham.Asset.Types (SomeAssetCard)
import Arkham.Card.CardCode
import Arkham.EncounterSet (EncounterSet)
import Arkham.Enemy.Types (SomeEnemyCard)
import Arkham.Homebrew.ContentEntries ()
import Arkham.Homebrew.TH
import Arkham.Homebrew.Types as X
import Arkham.Id (CampaignId)
import Arkham.Location.Types (SomeLocationCard)
import Arkham.Prelude ()
import Arkham.Treachery.Types (SomeTreacheryCard)

allHomebrewContent :: HomebrewContent
allHomebrewContent = $(discoverInstances ''IsHomebrewContent 'homebrewContent)

acts :: [SomeActCard]
acts = hcActs allHomebrewContent

agendas :: [SomeAgendaCard]
agendas = hcAgendas allHomebrewContent

assets :: [SomeAssetCard]
assets = hcAssets allHomebrewContent

enemies :: [SomeEnemyCard]
enemies = hcEnemies allHomebrewContent

locations :: [SomeLocationCard]
locations = hcLocations allHomebrewContent

treacheries :: [SomeTreacheryCard]
treacheries = hcTreacheries allHomebrewContent

scenarios :: [(CardCode, HomebrewScenario)]
scenarios = hcScenarios allHomebrewContent

scenarioSets :: [(CardCode, EncounterSet)]
scenarioSets = hcScenarioSets allHomebrewContent

campaigns :: [(CampaignId, HomebrewCampaign)]
campaigns = hcCampaigns allHomebrewContent
