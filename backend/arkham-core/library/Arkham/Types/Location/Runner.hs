module Arkham.Types.Location.Runner where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Source


import Arkham.Types.Asset.Uses
import Arkham.Types.Trait

type LocationRunner env
  = ( HasQueue env
    , HasCostPayment env
    , HasCount HorrorCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe OwnerId) env AssetId
    , HasId (Maybe StoryAssetId) env CardCode
    , HasId ActiveInvestigatorId env ()
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env InvestigatorId
    , HasList HandCard env InvestigatorId
    , HasList LocationName env ()
    , HasList UsedAbility env ()
    , HasModifiersFor env ()
    , HasName env LocationId
    , HasSet ActId env ()
    , HasSet AssetId env (InvestigatorId, UseType)
    , HasSet AssetId env InvestigatorId
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env Trait
    , HasSet EventId env ()
    , HasSet HandCardId env (InvestigatorId, PlayerCardType)
    , HasSet InvestigatorId env ()
    , HasSet LocationId env ()
    , HasSet LocationId env LocationMatcher
    , HasSet LocationId env [Trait]
    , HasSet Trait env Source
    , HasSet Trait env EnemyId
    , HasSet Trait env LocationId
    , HasSet UnrevealedLocationId env ()
    , HasSet UnrevealedLocationId env LocationMatcher
    )

