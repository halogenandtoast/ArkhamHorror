module Arkham.Treachery.CardDefs.TheCircleUndone where

import Arkham.Treachery.CardDefs.Import

rationalThought :: CardDef
rationalThought =
  (weakness "05008" "Rational Thought")
    { cdCardTraits = singleton Flaw
    }

terribleSecret :: CardDef
terribleSecret =
  (weakness "05015" "Terrible Secret")
    { cdCardTraits = singleton Madness
    , cdRevelation = CannotBeCanceledRevelation
    }

the13thVision :: CardDef
the13thVision =
  (basicWeakness "05041" "The 13th Vision")
    { cdCardTraits = singleton Omen
    }

-- Gravelight is the only card that cares which encounter deck it is drawn
-- from, so instead we represent it as two cards for which deck it is in.
