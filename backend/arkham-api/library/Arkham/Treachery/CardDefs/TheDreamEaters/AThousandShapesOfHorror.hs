module Arkham.Treachery.CardDefs.TheDreamEaters.AThousandShapesOfHorror where

import Arkham.Treachery.CardDefs.Import

deceptiveMemories :: CardDef
deceptiveMemories =
  (treachery "06193" "Deceptive Memories" AThousandShapesOfHorror 2)
    { cdCardTraits = singleton Terror
    }

endlessDescent :: CardDef
endlessDescent =
  (treachery "06190" "Endless Descent" AThousandShapesOfHorror 4)
    { cdCardTraits = singleton Curse
    }

glowingEyes :: CardDef
glowingEyes =
  (treachery "06192" "Glowing Eyes" AThousandShapesOfHorror 2)
    { cdCardTraits = singleton Terror
    }

indescribableApparition :: CardDef
indescribableApparition =
  (treachery "06191" "Indescribable Apparition" AThousandShapesOfHorror 2)
    { cdCardTraits = singleton Curse
    }

secretsInTheAttic :: CardDef
secretsInTheAttic =
  (treachery "06194" "Secrets in the Attic" AThousandShapesOfHorror 2)
    { cdCardTraits = singleton Scheme
    }
