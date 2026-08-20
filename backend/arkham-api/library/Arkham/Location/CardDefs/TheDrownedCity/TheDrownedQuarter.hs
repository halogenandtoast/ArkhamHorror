module Arkham.Location.CardDefs.TheDrownedCity.TheDrownedQuarter where

import Arkham.Location.CardDefs.Import

abyssalTrench :: CardDef
abyssalTrench =
  seaFloor "11541" "Abyssal Trench"

ancientGallery :: CardDef
ancientGallery =
  victory 1 $ seaFloor "11548" "Ancient Gallery"

barrierCoreActive :: CardDef
barrierCoreActive =
  otherSideIs "11540"
    $ location_ "11540b" ("Barrier Core" <:> "Active") [Seafloor, Central] TheDrownedQuarter

barrierCoreInactive :: CardDef
barrierCoreInactive =
  otherSideIs "11540b"
    $ location_ "11540" ("Barrier Core" <:> "Inactive") [Seafloor, Central] TheDrownedQuarter

blastedRuinsCrumblingEdifices :: CardDef
blastedRuinsCrumblingEdifices =
  seaFloor "11545" ("Blasted Ruins" <:> "Crumbling Edifices")

blastedRuinsSunkenCircle :: CardDef
blastedRuinsSunkenCircle =
  seaFloor "11544" ("Blasted Ruins" <:> "Sunken Circle")

coralReefFeedingGrounds :: CardDef
coralReefFeedingGrounds =
  victory 1 $ seaFloor "11547" ("Coral Reef" <:> "Feeding Grounds")

coralReefStatuaryGarden :: CardDef
coralReefStatuaryGarden =
  victory 1 $ seaFloor "11546" ("Coral Reef" <:> "Statuary Garden")

drownedAcropolisCollapsedRuins :: CardDef
drownedAcropolisCollapsedRuins =
  seaFloor "11543" ("Drowned Acropolis" <:> "Collapsed Ruins")

drownedAcropolisEphemeralRuins :: CardDef
drownedAcropolisEphemeralRuins =
  seaFloor "11542" ("Drowned Acropolis" <:> "Ephemeral Ruins")
