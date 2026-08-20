module Arkham.Treachery.CardDefs.BrethrenOfAsh.Hallucinations where

import Arkham.Treachery.CardDefs.Import

extraplanarVisions :: CardDef
extraplanarVisions =
  (treachery "12127" "Extraplanar Visions" Hallucinations 2)
    { cdCardTraits = singleton Power
    }

wildCompulsion :: CardDef
wildCompulsion =
  (treachery "12128" "Wild Compulsion" Hallucinations 2)
    { cdCardTraits = setFromList [Madness, Bane]
    }
