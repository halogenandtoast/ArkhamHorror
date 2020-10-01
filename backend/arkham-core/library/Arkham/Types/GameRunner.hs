module Arkham.Types.GameRunner where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude

type GameRunner env
  = ( HasQueue env
    , HasId LocationId InvestigatorId env
    , HasSet ConnectedLocationId LocationId env
    , HasSet EnemyId LocationId env
    , HasSet InvestigatorId LocationId env
    , HasSet TreacheryId LocationId env
    , HasCount ClueCount LocationId env
    , HasSet InvestigatorId () env
    , HasCount EnemyCount InvestigatorId env
    , HasCount ResourceCount InvestigatorId env
    , HasList DeckCard (InvestigatorId, Trait) env
    , HasId (Maybe OwnerId) AssetId env
    , HasSet EventId LocationId env
    , HasModifiersFor env InvestigatorId env
    )
