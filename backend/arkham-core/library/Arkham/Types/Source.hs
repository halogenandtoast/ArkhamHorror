module Arkham.Types.Source
  ( Source(..)
  , sourceIsAsset
  )
where

import ClassyPrelude

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
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Data.Aeson

data Source
  = AssetSource AssetId
  | EnemySource EnemyId
  | ScenarioSource ScenarioId
  | InvestigatorSource InvestigatorId
  | CardCodeSource CardCode
  | TokenSource TokenId
  | TokenEffectSource Token
  | AgendaSource AgendaId
  | LocationSource LocationId
  | SkillTestSource InvestigatorId SkillType Source (Maybe Action)
  | TreacherySource TreacheryId
  | EventSource EventId
  | SkillSource SkillId
  | EmptyDeckSource
  | DeckSource
  | GameSource
  | ActSource ActId
  | PlayerCardSource CardId
  | EncounterCardSource CardId
  | TestSource
  | DrawnTokenSource DrawnToken
  | ProxySource Source Source
  | EffectSource EffectId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

sourceIsAsset :: Source -> Bool
sourceIsAsset (AssetSource _) = True
sourceIsAsset _ = False
