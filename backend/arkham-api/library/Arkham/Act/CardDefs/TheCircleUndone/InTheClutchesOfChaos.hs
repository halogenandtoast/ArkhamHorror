module Arkham.Act.CardDefs.TheCircleUndone.InTheClutchesOfChaos where

import Arkham.Act.CardDefs.Import

darkKnowledgeV1 :: CardDef
darkKnowledgeV1 = (act "05286a" "Dark Knowledge (v. I)" 1 MusicOfTheDamned) {cdOtherSide = Just "05286b"}

darkKnowledgeV2 :: CardDef
darkKnowledgeV2 = (act "05288a" "Dark Knowledge (v. II)" 1 SecretsOfTheUniverse) {cdOtherSide = Just "05288b"}

beyondTheGrave :: CardDef
beyondTheGrave = act "05287" "Beyond the Grave" 2 MusicOfTheDamned

newWorldOrder :: CardDef
newWorldOrder = act "05289" "New World Order" 2 SecretsOfTheUniverse
