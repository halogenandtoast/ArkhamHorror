module Arkham.Location.CardDefs.TheBlobThatAteEverything where

import Arkham.Location.CardDefs.Import

bridge :: CardDef
bridge = location_ "85015" "Bridge" [Oozified] TheBlobThatAteEverything

church :: CardDef
church = location_ "85017" "Church" [Oozified] TheBlobThatAteEverything

desiccatedFarmland :: CardDef
desiccatedFarmland =
  (location_ "85020" "Desiccated Farmland" [Oozified] TheBlobThatAteEverything)
    { cdEncounterSetQuantity = Just 2
    }

fungusMound :: CardDef
fungusMound = location_ "85013" "Fungus Mound" [] TheBlobThatAteEverything

oozyLakebed :: CardDef
oozyLakebed =
  (location_ "85018" "Oozy Lakebed" [Oozified] TheBlobThatAteEverything)
    { cdEncounterSetQuantity = Just 2
    }

researchSite :: CardDef
researchSite =
  location_ "85011" "Research Site" [] TheBlobThatAteEverything

sewer :: CardDef
sewer = location_ "85014" "Sewer" [Oozified] TheBlobThatAteEverything

slimyStreets :: CardDef
slimyStreets =
  (location_ "85019" "Slimy Streets" [Oozified] TheBlobThatAteEverything)
    { cdEncounterSetQuantity = Just 2
    }

temporaryHQ :: CardDef
temporaryHQ = location_ "85012" "Temporary HQ" [] TheBlobThatAteEverything

theCrater :: CardDef
theCrater = location_ "85010" "The Crater" [Oozified] TheBlobThatAteEverything

waterTower :: CardDef
waterTower = location_ "85016" "Water Tower" [Oozified] TheBlobThatAteEverything
