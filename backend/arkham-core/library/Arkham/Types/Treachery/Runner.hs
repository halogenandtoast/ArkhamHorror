module Arkham.Types.Treachery.Runner where

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Trait

type TreacheryRunner env
  = ( HasQueue env
    , HasCount CardCount InvestigatorId env
    , HasCount ClueCount InvestigatorId env
    , HasCount ClueCount LocationId env
    , HasCount ResourceCount InvestigatorId env
    , HasCount Shroud LocationId env
    , HasCount SpendableClueCount InvestigatorId env
    , HasCount TreacheryCount (LocationId, CardCode) env
    , HasId LocationId InvestigatorId env
    , HasList UsedAbility () env
    , HasSet AssetId InvestigatorId env
    , HasSet ClosestEnemyId (LocationId, [Trait]) env
    , HasSet FarthestLocationId InvestigatorId env
    , HasSet LocationId () env
    , HasSet LocationId TreacheryCardCode env
    , HasSet LocationId [Trait] env
    , HasSet Trait LocationId env
    )

