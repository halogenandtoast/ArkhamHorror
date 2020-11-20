module Arkham.Types.Location.Runner where

import Arkham.Import

import Arkham.Types.Asset.Uses
import Arkham.Types.Trait

type LocationRunner env
  = ( HasCount env PlayerCount ()
    , HasQueue env
    , HasId (Maybe StoryAssetId) CardCode env
    , HasId (Maybe OwnerId) AssetId env
    , HasId ActiveInvestigatorId () env
    , HasId CardCode EnemyId env
    , HasId LocationId InvestigatorId env
    , HasSet LocationId env [Trait]
    , HasSet Trait env LocationId
    , HasSet EventId env ()
    , HasSet InvestigatorId env ()
    , HasSet Trait env EnemyId
    , HasSet EnemyId env Trait
    , HasSet ConnectedLocationId env LocationId
    , HasSet HandCardId env (InvestigatorId, PlayerCardType)
    , HasSet AssetId env InvestigatorId
    , HasSet AssetId env (InvestigatorId, UseType)
    , HasSet LocationId env ()
    , HasList LocationName () env
    , HasList HandCard InvestigatorId env
    , HasList UsedAbility () env
    , HasModifiersFor env env
    )

