module Arkham.Types.Source
  ( Source(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Action (Action)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait

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
  | SkillTestSource InvestigatorId SkillType Source Target (Maybe Action)
  | AfterSkillTestSource
  | TreacherySource TreacheryId
  | EventSource EventId
  | SkillSource SkillId
  | EmptyDeckSource
  | DeckSource
  | GameSource
  | CardIdSource CardId
  | ActSource ActId
  | PlayerCardSource CardId
  | EncounterCardSource CardId
  | TestSource (HashSet Trait)
  | ProxySource Source Source
  | AssetProxySource AssetMatcher Source
  | EffectSource EffectId
  | ResourceSource
  | AbilitySource Source Int
  | ActDeckSource
  | AgendaDeckSource
  | YouSource
  | NoSource
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
