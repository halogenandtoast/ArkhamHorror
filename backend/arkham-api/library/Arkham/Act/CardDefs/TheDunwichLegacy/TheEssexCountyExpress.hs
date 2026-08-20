module Arkham.Act.CardDefs.TheDunwichLegacy.TheEssexCountyExpress where

import Arkham.Act.CardDefs.Import

run :: CardDef
run = act "02165" "Run!" 1 TheEssexCountyExpress

getTheEngineRunning :: CardDef
getTheEngineRunning =
  act "02166" "Get the Engine Running!" 2 TheEssexCountyExpress
