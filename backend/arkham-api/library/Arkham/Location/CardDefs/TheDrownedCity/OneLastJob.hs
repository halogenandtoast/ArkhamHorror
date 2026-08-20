module Arkham.Location.CardDefs.TheDrownedCity.OneLastJob where

import Arkham.Location.CardDefs.Import

hibbsRoadhouse :: CardDef
hibbsRoadhouse =
  victory 1 $ otherSideIs "11504" $ location_ "11504b" "Hibb's Roadhouse" [Arkham, Front] OneLastJob

laBellaLunaTheDrownedCity :: CardDef
laBellaLunaTheDrownedCity =
  victory 1 $ otherSideIs "11505" $ location_ "11505b" "La Bella Luna" [Arkham, Front] OneLastJob

tillinghastEsoterica :: CardDef
tillinghastEsoterica =
  location_ "11509" ("Tillinghast Esoterica" <:> "Assorted Curiosities") [] OneLastJob
