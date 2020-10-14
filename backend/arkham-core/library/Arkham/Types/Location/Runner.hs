module Arkham.Types.Location.Runner where

import Arkham.Import

import Arkham.Types.Trait

type LocationRunner env
  = ( HasCount PlayerCount () env
    , HasQueue env
    , HasId (Maybe StoryAssetId) CardCode env
    , HasId (Maybe OwnerId) AssetId env
    , HasId CardCode EnemyId env
    , HasSet Trait LocationId env
    , HasSet EventId () env
    , HasSet Trait EnemyId env
    , HasModifiers env LocationId
    , HasSet ConnectedLocationId LocationId env
    , HasId ActiveInvestigatorId () env
    , HasSet HandCardId (InvestigatorId, PlayerCardType) env
    , HasSet AssetId InvestigatorId env
    )

