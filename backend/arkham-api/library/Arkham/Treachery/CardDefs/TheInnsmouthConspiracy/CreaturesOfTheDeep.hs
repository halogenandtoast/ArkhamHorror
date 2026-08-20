module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.CreaturesOfTheDeep where

import Arkham.Treachery.CardDefs.Import

deepOneAssault :: CardDef
deepOneAssault =
  (treachery "07090" "Deep One Assault" CreaturesOfTheDeep 2)
    { cdCardTraits = singleton Scheme
    }
