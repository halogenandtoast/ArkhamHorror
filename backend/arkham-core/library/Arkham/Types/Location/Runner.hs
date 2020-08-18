module Arkham.Types.Location.Runner where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Stats
import ClassyPrelude

type LocationActionRunner investigator
  = ( CanInvestigate LocationId investigator
    , CanMoveTo LocationId investigator
    , HasId InvestigatorId () investigator
    )

type LocationRunner env
  = ( HasCount PlayerCount () env
    , HasQueue env
    , HasId StoryAssetId CardCode env
    , HasInvestigatorStats Stats InvestigatorId env
    , HasId (Maybe OwnerId) AssetId env
    )

