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
import Arkham.Location.Types (SomeLocationCard)
import Arkham.Prelude ()
import Arkham.Story.Types (SomeStoryCard)
import Arkham.Treachery.Types (SomeTreacheryCard)

allHomebrewContent :: HomebrewContent
allHomebrewContent = $(discoverInstances ''IsHomebrewContent 'homebrewContent)

acts :: [SomeActCard]
acts = allHomebrewContent.acts

agendas :: [SomeAgendaCard]
agendas = allHomebrewContent.agendas

assets :: [SomeAssetCard]
assets = allHomebrewContent.assets

enemies :: [SomeEnemyCard]
enemies = allHomebrewContent.enemies

locations :: [SomeLocationCard]
locations = allHomebrewContent.locations

stories :: [SomeStoryCard]
stories = allHomebrewContent.stories

treacheries :: [SomeTreacheryCard]
treacheries = allHomebrewContent.treacheries

scenarios :: HomebrewScenarios
scenarios = allHomebrewContent.scenarios

scenarioSets :: [(CardCode, EncounterSet)]
scenarioSets = [(cardCode, encounterSet) | (cardCode, HomebrewScenario encounterSet _) <- scenarios]

campaigns :: HomebrewCampaigns
campaigns = allHomebrewContent.campaigns
