module Arkham.Treachery.CardDefs.TracesToNowhere where

import Arkham.Treachery.CardDefs.Import

aethrealBlight :: CardDef
aethrealBlight =
  (treachery "73056" "Aethereal Blight" DimensionalMadness 2)
    { cdCardTraits = setFromList [Hazard, Otherworld]
    }

approachingShadow :: CardDef
approachingShadow =
  (treachery "73045" "Approaching Shadow" TracesToNowhere 2)
    { cdCardTraits = singleton Scheme
    }

backToSquareOne :: CardDef
backToSquareOne =
  (treachery "73042" "Back to Square One" TracesToNowhere 3)
    { cdCardTraits = singleton Blunder
    }

disappearingEvidence :: CardDef
disappearingEvidence =
  (treachery "73047" "Disappearing Evidence" TracesToNowhere 2)
    { cdCardTraits = singleton Scheme
    }

drawingHeat :: CardDef
drawingHeat =
  (treachery "73044" "Drawing Heat" TracesToNowhere 2)
    { cdCardTraits = singleton Scheme
    }

heavyRain :: CardDef
heavyRain =
  (treachery "73043" "Heavy Rain" TracesToNowhere 2)
    { cdCardTraits = singleton Hazard
    }

hungryVoid :: CardDef
hungryVoid =
  peril
    $ (treachery "73058" "Hungry Void" DimensionalMadness 2)
      { cdCardTraits = singleton Power
      }

lockedOut :: CardDef
lockedOut =
  (treachery "73048" "Locked Out" TracesToNowhere 2)
    { cdCardTraits = singleton Scheme
    }

lostToTheDarkness :: CardDef
lostToTheDarkness =
  (treachery "73057" "Lost to the Darkness" DimensionalMadness 2)
    { cdCardTraits = singleton Power
    }

overYourShoulder :: CardDef
overYourShoulder =
  peril
    $ (treachery "73046" "Over Your Shoulder" TracesToNowhere 2)
      { cdCardTraits = singleton Terror
      }

theMysteryDeepens :: CardDef
theMysteryDeepens =
  (treachery "73041" "The Mystery Deepens" TracesToNowhere 3)
    { cdCardTraits = singleton Scheme
    }
