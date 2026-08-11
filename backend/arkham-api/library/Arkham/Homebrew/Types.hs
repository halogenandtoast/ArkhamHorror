{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Homebrew.Types where

import Arkham.Act.Types (SomeActCard)
import Arkham.Agenda.Types (SomeAgendaCard)
import Arkham.Asset.Types (SomeAssetCard)
import Arkham.Campaign.Types (IsCampaign)
import Arkham.Card.CardCode
import Arkham.Difficulty
import Arkham.EncounterSet (EncounterSet)
import Arkham.Enemy.Types (SomeEnemyCard)
import Arkham.Id (CampaignId)
import Arkham.Location.Types (SomeLocationCard)
import Arkham.Prelude
import Arkham.Scenario.Types (IsScenario)
import Arkham.Story.Types (SomeStoryCard)
import Arkham.Treachery.Types (SomeTreacheryCard)

data HomebrewCampaign = forall a. IsCampaign a => HomebrewCampaign (Difficulty -> a)

data HomebrewScenario = forall a. IsScenario a => HomebrewScenario EncounterSet (Difficulty -> a)

type HomebrewScenarios = [(CardCode, HomebrewScenario)]

type HomebrewCampaigns = [(CampaignId, HomebrewCampaign)]

{- | Everything a homebrew campaign (or standalone) contributes at runtime:
entity implementations plus campaign/scenario registration.
-}
data HomebrewContent = HomebrewContent
  { acts :: [SomeActCard]
  , agendas :: [SomeAgendaCard]
  , assets :: [SomeAssetCard]
  , enemies :: [SomeEnemyCard]
  , locations :: [SomeLocationCard]
  , stories :: [SomeStoryCard]
  , treacheries :: [SomeTreacheryCard]
  , scenarios :: HomebrewScenarios
  , campaigns :: HomebrewCampaigns
  }

instance Semigroup HomebrewContent where
  a <> b =
    HomebrewContent
      { acts = a.acts <> b.acts
      , agendas = a.agendas <> b.agendas
      , assets = a.assets <> b.assets
      , enemies = a.enemies <> b.enemies
      , locations = a.locations <> b.locations
      , stories = a.stories <> b.stories
      , treacheries = a.treacheries <> b.treacheries
      , scenarios = a.scenarios <> b.scenarios
      , campaigns = a.campaigns <> b.campaigns
      }

instance Monoid HomebrewContent where
  mempty = HomebrewContent [] [] [] [] [] [] [] [] []

{- | Implement in your campaign's @Content.hs@ on a campaign-local tag type;
the instance is discovered automatically (see 'Arkham.Homebrew.Registry').
-}
class IsHomebrewContent a where
  homebrewContent :: HomebrewContent
