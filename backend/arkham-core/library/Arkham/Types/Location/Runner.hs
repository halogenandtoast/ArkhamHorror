module Arkham.Types.Location.Runner where

import Arkham.Import

import Arkham.Types.Asset.Uses
import Arkham.Types.Trait

type LocationRunner env
  = ( HasCount PlayerCount () env
    , HasQueue env
    , HasId (Maybe StoryAssetId) CardCode env
    , HasId (Maybe OwnerId) AssetId env
    , HasId CardCode EnemyId env
    , HasId LocationId InvestigatorId env
    , HasSet LocationId [Trait] env
    , HasSet Trait LocationId env
    , HasSet EventId () env
    , HasSet InvestigatorId () env
    , HasSet Trait EnemyId env
    , HasSet EnemyId Trait env
    , HasModifiers env LocationId
    , HasSet ConnectedLocationId LocationId env
    , HasId ActiveInvestigatorId () env
    , HasSet HandCardId (InvestigatorId, PlayerCardType) env
    , HasSet AssetId InvestigatorId env
    , HasSet AssetId (InvestigatorId, UseType) env
    , HasSet LocationId () env
    , HasList LocationName () env
    , HasList HandCard InvestigatorId env
    , HasList UsedAbility () env
    )

