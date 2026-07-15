{-# LANGUAGE AllowAmbiguousTypes #-}

module Arkham.Homebrew.Types where

import Arkham.Act.Types (SomeActCard)
import Arkham.Agenda.Types (SomeAgendaCard)
import Arkham.Asset.Types (SomeAssetCard)
import Arkham.Campaign.Types (IsCampaign)
import Arkham.Card.CardCode
import Arkham.EncounterSet (EncounterSet)
import Arkham.Enemy.Types (SomeEnemyCard)
import Arkham.Id (CampaignId)
import Arkham.Location.Types (SomeLocationCard)
import Arkham.Prelude
import Arkham.Treachery.Types (SomeTreacheryCard)
import Arkham.Difficulty
import Arkham.Scenario.Types (IsScenario)

data HomebrewCampaign = forall a. IsCampaign a => HomebrewCampaign (Difficulty -> a)

data HomebrewScenario = forall a. IsScenario a => HomebrewScenario (Difficulty -> a)

-- | Everything a homebrew campaign (or standalone) contributes at runtime:
-- entity implementations plus campaign/scenario registration.
data HomebrewContent = HomebrewContent
  { hcActs :: [SomeActCard]
  , hcAgendas :: [SomeAgendaCard]
  , hcAssets :: [SomeAssetCard]
  , hcEnemies :: [SomeEnemyCard]
  , hcLocations :: [SomeLocationCard]
  , hcTreacheries :: [SomeTreacheryCard]
  , hcScenarios :: [(CardCode, HomebrewScenario)]
  , hcScenarioSets :: [(CardCode, EncounterSet)]
  , hcCampaigns :: [(CampaignId, HomebrewCampaign)]
  }

instance Semigroup HomebrewContent where
  a <> b =
    HomebrewContent
      { hcActs = hcActs a <> hcActs b
      , hcAgendas = hcAgendas a <> hcAgendas b
      , hcAssets = hcAssets a <> hcAssets b
      , hcEnemies = hcEnemies a <> hcEnemies b
      , hcLocations = hcLocations a <> hcLocations b
      , hcTreacheries = hcTreacheries a <> hcTreacheries b
      , hcScenarios = hcScenarios a <> hcScenarios b
      , hcScenarioSets = hcScenarioSets a <> hcScenarioSets b
      , hcCampaigns = hcCampaigns a <> hcCampaigns b
      }

instance Monoid HomebrewContent where
  mempty = HomebrewContent [] [] [] [] [] [] [] [] []

-- | Implement in your campaign's @Content.hs@ on a campaign-local tag type;
-- the instance is discovered automatically (see 'Arkham.Homebrew.Registry').
class IsHomebrewContent a where
  homebrewContent :: HomebrewContent
