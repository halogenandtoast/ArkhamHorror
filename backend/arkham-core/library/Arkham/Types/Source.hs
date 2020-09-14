module Arkham.Types.Source
  ( Source(..)
  , sourceIsAsset
  )
where

import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card.Id
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.SkillId
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson

sourceIsAsset :: Source -> Bool
sourceIsAsset (AssetSource _) = True
sourceIsAsset _ = False

data Source
  = AssetSource AssetId
  | EnemySource EnemyId
  | InvestigatorSource InvestigatorId
  | TokenSource Token
  | AgendaSource AgendaId
  | LocationSource LocationId
  | SkillTestSource
  | TreacherySource TreacheryId
  | EventSource EventId
  | SkillSource SkillId
  | EmptyDeckSource
  | DeckSource
  | ActSource ActId
  | PlayerCardSource CardId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
