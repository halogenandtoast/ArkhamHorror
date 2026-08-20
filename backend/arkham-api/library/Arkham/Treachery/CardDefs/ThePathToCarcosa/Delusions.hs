module Arkham.Treachery.CardDefs.ThePathToCarcosa.Delusions where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

descentIntoMadness :: CardDef
descentIntoMadness =
  (treachery "03085" "Descent into Madness" Delusions 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Surge
    }

whispersInYourHeadAnxiety :: CardDef
whispersInYourHeadAnxiety =
  (treachery "03084c" ("Whispers in Your Head" <:> "Anxiety") Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

whispersInYourHeadDismay :: CardDef
whispersInYourHeadDismay =
  (treachery "03084a" ("Whispers in Your Head" <:> "Dismay") Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

whispersInYourHeadDoubt :: CardDef
whispersInYourHeadDoubt =
  (treachery "03084d" ("Whispers in Your Head" <:> "Doubt") Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

whispersInYourHeadDread :: CardDef
whispersInYourHeadDread =
  (treachery "03084b" ("Whispers in Your Head" <:> "Dread") Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }
