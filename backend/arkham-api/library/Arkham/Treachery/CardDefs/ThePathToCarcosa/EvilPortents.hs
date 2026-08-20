module Arkham.Treachery.CardDefs.ThePathToCarcosa.EvilPortents where

import Arkham.Treachery.CardDefs.Import

blackStarsRise :: CardDef
blackStarsRise =
  (treachery "03090" "Black Stars Rise" EvilPortents 2)
    { cdCardTraits = singleton Omen
    }

spiresOfCarcosa :: CardDef
spiresOfCarcosa =
  (treachery "03091" "Spires of Carcosa" EvilPortents 2)
    { cdCardTraits = singleton Omen
    }

twistedToHisWill :: CardDef
twistedToHisWill =
  (treachery "03092" "Twisted to His Will" EvilPortents 2)
    { cdCardTraits = singleton Pact
    }
