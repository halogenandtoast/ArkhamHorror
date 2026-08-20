module Arkham.Treachery.CardDefs.TheCircleUndone.BeforeTheBlackThrone where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

aWorldInDarkness :: CardDef
aWorldInDarkness =
  (treachery "05345" "A World in Darkness" BeforeTheBlackThrone 2)
    { cdCardTraits = singleton Endtimes
    }

theEndIsNigh :: CardDef
theEndIsNigh =
  (treachery "05344" "The End is Nigh!" BeforeTheBlackThrone 2)
    { cdCardTraits = singleton Endtimes
    }

ultimateChaos :: CardDef
ultimateChaos =
  (treachery "05342" "Ultimate Chaos" BeforeTheBlackThrone 3)
    { cdCardTraits = singleton Power
    , cdRevelation = CannotBeCanceledRevelation
    }

whisperedBargain :: CardDef
whisperedBargain =
  (treachery "05343" "Whispered Bargain" BeforeTheBlackThrone 2)
    { cdCardTraits = singleton Pact
    , cdKeywords = singleton Keyword.Peril
    }
