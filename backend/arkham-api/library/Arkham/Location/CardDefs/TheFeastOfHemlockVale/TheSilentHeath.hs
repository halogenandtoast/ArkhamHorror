module Arkham.Location.CardDefs.TheFeastOfHemlockVale.TheSilentHeath where

import Arkham.Location.CardDefs.Import

ashenSlope :: CardDef
ashenSlope = location "10557" "Ashen Slope" [Blight] Circle [Triangle, Equals] TheSilentHeath

crystalGrove :: CardDef
crystalGrove = location "10555" "Crystal Grove" [Blight] Triangle [Equals, Circle] TheSilentHeath

crystalNursery :: CardDef
crystalNursery =
  revelation $ singleSided $ location_ "10560" "Crystal Nursery" [Cave, Lair, Blight] TheSilentHeath

larvalTunnel :: CardDef
larvalTunnel = revelation $ singleSided $ location_ "10559" "Larval Tunnel" [Cave, Lair] TheSilentHeath

pearlEstateRuins :: CardDef
pearlEstateRuins = location "10556" "Pearl Estate Ruins" [Ruins] Equals [Triangle, Circle] TheSilentHeath

saltChamber :: CardDef
saltChamber = revelation $ singleSided $ location_ "10558" "Salt Chamber" [Cave, Lair] TheSilentHeath
