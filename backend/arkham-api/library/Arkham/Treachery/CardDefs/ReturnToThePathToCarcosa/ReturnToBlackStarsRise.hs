module Arkham.Treachery.CardDefs.ReturnToThePathToCarcosa.ReturnToBlackStarsRise where

import Arkham.Treachery.CardDefs.Import

hastursGaze :: CardDef
hastursGaze =
  peril
    $ hidden
    $ (treachery "52057" "Hastur's Gaze" ReturnToBlackStarsRise 1)
      { cdCardTraits = setFromList [Power]
      }

hastursGrasp :: CardDef
hastursGrasp =
  peril
    $ hidden
    $ (treachery "52058" "Hastur's Grasp" ReturnToBlackStarsRise 1)
      { cdCardTraits = setFromList [Power]
      }
