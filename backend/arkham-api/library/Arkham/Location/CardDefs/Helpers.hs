{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Shared constructors that several location card-def modules build on.
module Arkham.Location.CardDefs.Helpers where

import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Direction
import Arkham.EncounterSet hiding (
  Arkham,
  Blight,
  Byakhee,
  Dreamlands,
  Dunwich,
  Expedition,
  Poison,
  Rlyeh,
  StarSpawn,
 )
import Arkham.GameValue
import Arkham.Location.CardDefs.Base
import Arkham.LocationSymbol
import Arkham.Name
import Arkham.Prelude
import Arkham.Trait hiding (Circle)

crumblingArchives :: Name
crumblingArchives = "Crumbling Archives"

oldInnsmouthRoad :: CardCode -> Name -> CardDef
oldInnsmouthRoad cardCode name =
  locationWithUnrevealed_
    cardCode
    "Old Innsmouth Road"
    [Road]
    name
    [Road]
    HorrorInHighGear

railIcons :: [GridDirection] -> CardDef -> CardDef
railIcons dirs def =
  def
    { cdMeta = mapFromList [("rails", toJSON dirs)]
    }

seaFloor :: CardCode -> Name -> CardDef
seaFloor cardCode name =
  locationWithUnrevealed_ cardCode "Sea Floor" [Seafloor] name [Seafloor] TheDrownedQuarter

summit :: Name
summit = "Summit"

vaultChamber :: CardCode -> Name -> [Trait] -> CardDef
vaultChamber cardCode name traits =
  locationWithUnrevealed_ cardCode "Vault Chamber" [Rlyeh, Vault] name traits TheGrandVault
