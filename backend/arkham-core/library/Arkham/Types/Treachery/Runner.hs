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
    , HasSet AssetId InvestigatorId env
    , HasSet ClosestEnemyId (LocationId, [Trait]) env
    , HasSet ConnectedLocationId LocationId env
    , HasSet DiscardableAssetId InvestigatorId env
    , HasSet FarthestLocationId InvestigatorId env
    , HasSet InvestigatorId LocationId env
    , HasSet InvestigatorId () env
    , HasSet LocationId () env
    , HasSet LocationId TreacheryCardCode env
    , HasSet LocationId [Trait] env
    , HasSet Trait LocationId env
    , HasSet FarthestEnemyId (InvestigatorId, EnemyTrait) env
    , HasSet UniqueEnemyId () env
    , HasSet EnemyId () env
    )

