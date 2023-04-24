{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Arkham.Source (
  Source (..),
) where

import Arkham.Prelude

import Arkham.Action (Action)
import {-# SOURCE #-} Arkham.Card
import Arkham.Card.CardCode
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Card.PlayerCard
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillTest.Type
import Arkham.Token
import Arkham.Trait
import Data.Aeson.TH

data Source
  = AbilitySource Source Int
  | ActDeckSource
  | ActSource ActId
  | AfterSkillTestSource
  | AgendaDeckSource
  | AgendaSource AgendaId
  | AgendaMatcherSource AgendaMatcher
  | AssetMatcherSource AssetMatcher
  | AssetSource AssetId
  | CardCodeSource CardCode
  | CardSource Card
  | DeckSource
  | EffectSource EffectId
  | EmptyDeckSource
  | EncounterCardSource CardId
  | EnemyAttackSource EnemyId
  | EnemySource EnemyId
  | EventSource EventId
  | GameSource
  | InvestigatorSource InvestigatorId
  | LocationMatcherSource LocationMatcher
  | EnemyMatcherSource EnemyMatcher
  | LocationSource LocationId
  | PlayerCardSource PlayerCard
  | ProxySource { source :: Source, originalSource :: Source }
  | ResourceSource
  | ScenarioSource
  | SkillSource SkillId
  | SkillTestSource InvestigatorId SkillTestType Source (Maybe Action)
  | StorySource CardCode
  | TestSource (Set Trait)
  | TokenEffectSource TokenFace
  | TokenSource Token
  | TreacherySource TreacheryId
  | YouSource
  | CampaignSource
  | ThisCard
  | CardCostSource CardId
  | BothSource Source Source
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions ''Source)

instance ToJSONKey Source
instance FromJSONKey Source
