module Arkham.Treachery.CardDefs.TheCircleUndone.TheSecretName where

import Arkham.Treachery.CardDefs.Import

disquietingDreams :: CardDef
disquietingDreams =
  (treachery "05147" "Disquieting Dreams" TheSecretName 2)
    { cdCardTraits = singleton Terror
    }

extradimensionalVisions :: CardDef
extradimensionalVisions =
  (treachery "05145" "Extradimensional Visions" TheSecretName 2)
    { cdCardTraits = singleton Hex
    }

ghostlyPresence :: CardDef
ghostlyPresence =
  (treachery "05144" "Ghostly Presence" TheSecretName 2)
    { cdCardTraits = singleton Omen
    }

meddlesomeFamiliar :: CardDef
meddlesomeFamiliar =
  (treachery "05143" "Meddlesome Familiar" TheSecretName 3)
    { cdCardTraits = singleton Curse
    }

pulledByTheStars :: CardDef
pulledByTheStars =
  (treachery "05146" "Pulled by the Stars" TheSecretName 2)
    { cdCardTraits = singleton Hex
    }
