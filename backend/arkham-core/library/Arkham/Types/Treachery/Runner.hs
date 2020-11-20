module Arkham.Types.Treachery.Runner where

import ClassyPrelude

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
    , HasCount env ActsRemainingCount ()
    , HasCount env CardCount InvestigatorId
    , HasCount env ClueCount InvestigatorId
    , HasCount env ClueCount LocationId
    , HasCount env PlayerCount ()
    , HasCount env ResourceCount InvestigatorId
    , HasCount env Shroud LocationId
    , HasCount env SpendableClueCount InvestigatorId
    , HasCount env TreacheryCount (LocationId, CardCode)
    , HasId (Maybe StoryEnemyId) CardCode env
    , HasId LocationId EnemyId env
    , HasId LocationId InvestigatorId env
    , HasList UsedAbility () env
    , HasSet AssetId env InvestigatorId
    , HasSet ClosestEnemyId env (LocationId, [Trait])
    , HasSet ConnectedLocationId env LocationId
    , HasSet DiscardableAssetId env InvestigatorId
    , HasSet FarthestLocationId env InvestigatorId
    , HasSet InvestigatorId env LocationId
    , HasSet InvestigatorId env ()
    , HasSet LocationId env ()
    , HasSet LocationId env TreacheryCardCode
    , HasSet LocationId env [Trait]
    , HasSet Trait env LocationId
    , HasSet FarthestEnemyId env (InvestigatorId, EnemyTrait)
    , HasSet UniqueEnemyId env ()
    , HasSet EnemyId env ()
    )

