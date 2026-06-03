module Arkham.Treachery.CardDefs.TheDrownedCity where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword

illDoItMyself :: CardDef
illDoItMyself = (weakness "11003" "\"I'll do it myself\"") {cdCardTraits = setFromList [Flaw]}

dreamsOfTheFlood :: CardDef
dreamsOfTheFlood =
  (weakness "11006" "Dreams of the Flood") {cdCardTraits = setFromList [Omen, Endtimes]}

glimpseTheVoid :: CardDef
glimpseTheVoid =
  (weakness "11010" "Glimpse the Void") {cdCardTraits = setFromList [Blunder, Insight]}

confiscation :: CardDef
confiscation = (weakness "11013" "Confiscation") {cdCardTraits = setFromList [Blunder]}

prophecyOfTheEnd :: CardDef
prophecyOfTheEnd =
  (weakness "11016" "Prophecy of the End")
    { cdCardTraits = setFromList [Omen, Endtimes]
    , cdKeywords = setFromList [Keyword.Surge]
    , cdCardType = TreacheryType
    }

castAdrift :: CardDef
castAdrift = (weakness "11019" "Cast Adrift") {cdCardTraits = setFromList [Hardship]}

downAndOut :: CardDef
downAndOut = (basicWeakness "11126" "Down and Out") {cdCardTraits = setFromList [Hardship]}

morbidCuriosity :: CardDef
morbidCuriosity = (basicWeakness "11127" "Morbid Curiosity") {cdCardTraits = setFromList [Flaw]}

disruptivePoltergeist :: CardDef
disruptivePoltergeist = (basicWeakness "11128" "Disruptive Poltergeist") {cdCardTraits = setFromList [Curse]}

frenzied :: CardDef
frenzied = (basicWeakness "11129" "Frenzied") {cdCardTraits = setFromList [Madness]}
