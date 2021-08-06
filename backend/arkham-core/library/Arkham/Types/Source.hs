module Arkham.Types.Source
  ( Source(..)
  ) where

import Arkham.Prelude

import Arkham.Types.ActId
import Arkham.Types.Action (Action)
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.EffectId
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId

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
  | TestSource (Set Trait)
  | ProxySource Source Source
  | EffectSource EffectId
  | ResourceSource
  | AbilitySource Source Int
  | ActDeckSource
  | AgendaDeckSource
  deriving stock (Ord, Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
