module Arkham.Skill.CardDefs.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.Id
import Arkham.Name
import Arkham.Prelude
import Arkham.SkillType

skill :: CardCode -> Name -> [SkillIcon] -> ClassSymbol -> CardDef
skill cardCode name icons classSymbol =
  (emptyCardDef cardCode name SkillType)
    { cdClassSymbols = singleton classSymbol
    , cdSkills = icons
    , cdCanCommitWhenNoIcons = null icons
    }

signature :: InvestigatorId -> CardDef -> CardDef
signature iid cd = cd {cdDeckRestrictions = [Signature iid], cdLevel = Nothing}
