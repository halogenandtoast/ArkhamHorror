{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Arkham.Source (
  Source (..),
) where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Card.CardCode
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Card.PlayerCard
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Token
import Arkham.Trait

data Source
  = AbilitySource Source Int
  | ActDeckSource
  | ActSource ActId
  | AfterSkillTestSource
  | AgendaDeckSource
  | AgendaSource AgendaId
  | AssetMatcherSource AssetMatcher
  | AssetSource AssetId
  | CardCodeSource CardCode
  | CardIdSource CardId
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
  | ScenarioSource ScenarioId
  | SkillSource SkillId
  | SkillTestSource InvestigatorId SkillType Source (Maybe Action)
  | StorySource CardCode
  | TestSource (HashSet Trait)
  | TokenEffectSource TokenFace
  | TokenSource Token
  | TreacherySource TreacheryId
  | YouSource
  | CampaignSource
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
