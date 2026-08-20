module Arkham.Act.CardDefs.ThePathToCarcosa.DimCarcosa where

import Arkham.Act.CardDefs.Import

inLostCarcosa :: CardDef
inLostCarcosa = act "03320" "In Lost Carcosa" 1 DimCarcosa

searchForTheStrangerV1 :: CardDef
searchForTheStrangerV1 =
  (act "03321a" "Search For the Stranger (v.I)" 2 DimCarcosa)
    { cdOtherSide = Just "03321b"
    }

searchForTheStrangerV2 :: CardDef
searchForTheStrangerV2 =
  (act "03322a" "Search For the Stranger (v.II)" 2 DimCarcosa)
    { cdOtherSide = Just "03322ab"
    }

searchForTheStrangerV3 :: CardDef
searchForTheStrangerV3 =
  (act "03323a" "Search For the Stranger (v.III)" 2 DimCarcosa)
    { cdOtherSide = Just "03323ab"
    }

theKingInTatters :: CardDef
theKingInTatters = act "03324" "The King in Tatters" 3 DimCarcosa
