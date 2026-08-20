module Arkham.Treachery.CardDefs.ReturnToThePathToCarcosa.ReturnToTheLastKing where

import Arkham.Treachery.CardDefs.Import

shockingDisplay :: CardDef
shockingDisplay =
  (treachery "52027" "Shocking Display" ReturnToTheLastKing 1)
    { cdCardTraits = setFromList [Terror]
    , cdVictoryPoints = Just 0
    }
