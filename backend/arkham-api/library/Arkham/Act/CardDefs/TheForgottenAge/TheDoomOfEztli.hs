module Arkham.Act.CardDefs.TheForgottenAge.TheDoomOfEztli where

import Arkham.Act.CardDefs.Import

intoTheRuins :: CardDef
intoTheRuins =
  (act "04057" "Into the Ruins" 1 TheDoomOfEztli) {cdVengeancePoints = Just 1}

magicAndScience :: CardDef
magicAndScience = act "04058" "Magic and Science" 2 TheDoomOfEztli

escapeTheRuins :: CardDef
escapeTheRuins = act "04059" "Escape the Ruins" 3 TheDoomOfEztli
