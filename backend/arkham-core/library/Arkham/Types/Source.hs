module Arkham.Types.Source
  ( Source(..)
  , sourceIsEvent
  , sourceIsAsset
  )
where

import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson

sourceIsEvent :: Source -> Bool
sourceIsEvent (EventSource _) = True
sourceIsEvent _ = False

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
  | EventSource CardCode
  | SkillSource CardCode
  | EmptyDeckSource
  | DeckSource
  | ActSource ActId
  | PlayerCardSource CardId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
