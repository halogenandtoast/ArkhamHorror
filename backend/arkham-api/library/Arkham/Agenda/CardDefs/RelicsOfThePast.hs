module Arkham.Agenda.CardDefs.RelicsOfThePast where

import Arkham.Agenda.CardDefs.Import

somethingElseStirs :: CardDef
somethingElseStirs =
  (agenda "90066a" "Something Else Stirs..." 1 RelicsOfThePast) {cdOtherSide = Just "90066b"}

guardianOfTheRelics :: CardDef
guardianOfTheRelics = agenda "90067" "Guardian of the Relics" 2 RelicsOfThePast
