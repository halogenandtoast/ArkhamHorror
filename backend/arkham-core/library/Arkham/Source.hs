module Arkham.Source (
  Source (..),
) where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Card.CardCode
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Card.PlayerCard
import Arkham.EffectId
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Token
import Arkham.Trait

data Source
  = AssetSource AssetId
  | EnemySource EnemyId
  | ScenarioSource ScenarioId
  | InvestigatorSource InvestigatorId
  | CardCodeSource CardCode
  | TokenSource Token
  | TokenEffectSource TokenFace
  | AgendaSource AgendaId
  | LocationSource LocationId
  | SkillTestSource InvestigatorId SkillType Source (Maybe Action)
  | AfterSkillTestSource
  | TreacherySource TreacheryId
  | EventSource EventId
  | SkillSource SkillId
  | EmptyDeckSource
  | DeckSource
  | GameSource
  | CardIdSource CardId
  | ActSource ActId
  | PlayerCardSource PlayerCard
  | EncounterCardSource CardId
  | TestSource (HashSet Trait)
  | ProxySource Source Source
  | EffectSource EffectId
  | ResourceSource
  | AbilitySource Source Int
  | ActDeckSource
  | AgendaDeckSource
  | YouSource
  | EnemyAttackSource EnemyId
  | AssetMatcherSource AssetMatcher
  | LocationMatcherSource LocationMatcher
  | StorySource CardCode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
