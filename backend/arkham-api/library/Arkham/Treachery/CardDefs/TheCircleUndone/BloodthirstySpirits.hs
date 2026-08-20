module Arkham.Treachery.CardDefs.TheCircleUndone.BloodthirstySpirits where

import Arkham.Treachery.CardDefs.Import

bloodthirstySpirits :: CardDef
bloodthirstySpirits =
  (treachery "54075" "Bloodthirsty Spiris" BloodthirstySpirits 2)
    { cdCardTraits = setFromList [Terror, Spectral]
    }
