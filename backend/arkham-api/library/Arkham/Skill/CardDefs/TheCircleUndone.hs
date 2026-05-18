module Arkham.Skill.CardDefs.TheCircleUndone where

import Arkham.Skill.CardDefs.Import

steadfast :: CardDef
steadfast =
  (skill "05022" "Steadfast" [#willpower, #combat] Guardian)
    { cdCardTraits = singleton Innate
    , cdOutOfPlayEffects = [InHandEffect]
    }

curiosity :: CardDef
curiosity =
  (skill "05026" "Curiosity" [#willpower, #intellect] Seeker)
    { cdCardTraits = singleton Innate
    }

cunning :: CardDef
cunning =
  (skill "05030" "Cunning" [#intellect, #agility] Rogue)
    { cdCardTraits = singleton Innate
    }

prophesy :: CardDef
prophesy =
  (skill "05034" "Prophesy" [#wild] Mystic)
    { cdCardTraits = singleton Practiced
    }

ableBodied :: CardDef
ableBodied =
  (skill "05038" "Able Bodied" [#combat, #agility] Survivor)
    { cdCardTraits = singleton Innate
    , cdOutOfPlayEffects = [InHandEffect]
    }
