module Arkham.Event.Cards.Core2026 where

import Arkham.Capability
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

detectivesIntuition :: CardDef
detectivesIntuition =
  signature "12004"
    $ (event "12005" "Detective's Intuition" 0 Seeker)
      { cdCardTraits = setFromList [Insight]
      , cdSkills = [#willpower, #intellect, #wild]
      , cdCriteria =
          Just
            $ Criteria.youExist
            $ oneOf [can.gain.resources, can.heal.damage ThisCard, can.heal.horror ThisCard]
      , cdOutOfPlayEffects = [InHandEffect]
      }

deadEnds :: CardDef
deadEnds =
  signature "12004"
    $ (event "12006" "Dead Ends" 5 Neutral)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Blunder]
      , cdOutOfPlayEffects = [InHandEffect, InSearchEffect]
      }
