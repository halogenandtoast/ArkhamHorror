module Arkham.Treachery.CardDefs.ThePathToCarcosa.DimCarcosa where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

dismalCurse :: CardDef
dismalCurse =
  (treachery "03337" "Dismal Curse" DimCarcosa 3)
    { cdCardTraits = setFromList [Curse, Terror]
    }

possessionMurderous :: CardDef
possessionMurderous =
  (treachery "03342" ("Possession" <:> "Murderous") DimCarcosa 1)
    { cdCardTraits = setFromList [Hex, Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

possessionTorturous :: CardDef
possessionTorturous =
  (treachery "03341" ("Possession" <:> "Torturous") DimCarcosa 1)
    { cdCardTraits = setFromList [Hex, Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

possessionTraitorous :: CardDef
possessionTraitorous =
  (treachery "03340" ("Possession" <:> "Traitorous") DimCarcosa 1)
    { cdCardTraits = setFromList [Hex, Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    , cdCommitRestrictions = [CommittableTreachery]
    }

realmOfMadness :: CardDef
realmOfMadness =
  (treachery "03338" "Realm of Madness" DimCarcosa 2)
    { cdCardTraits = singleton Terror
    }

theFinalAct :: CardDef
theFinalAct =
  (treachery "03339" "The Final Act" DimCarcosa 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Surge]
    }
