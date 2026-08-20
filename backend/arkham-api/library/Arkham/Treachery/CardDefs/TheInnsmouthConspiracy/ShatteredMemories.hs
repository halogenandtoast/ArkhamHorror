module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.ShatteredMemories where

import Arkham.Treachery.CardDefs.Import

fracturedConsciousness :: CardDef
fracturedConsciousness =
  (treachery "07097" "Fractured Consciousness" ShatteredMemories 2)
    { cdCardTraits = singleton Terror
    }

macabreMemento :: CardDef
macabreMemento =
  (treachery "07096" "Macabre Memento" ShatteredMemories 2)
    { cdCardTraits = singleton Terror
    }

memoryOfOblivion :: CardDef
memoryOfOblivion =
  (treachery "07098" "Memory of Oblivion" ShatteredMemories 2)
    { cdCardTraits = singleton Terror
    }
