module Arkham.Asset.Cards.Import (module Arkham.Asset.Cards.Import, module X) where

import Arkham.Asset.Cards.Base as X
import Arkham.Asset.Uses as X hiding (Key, Lead)
import Arkham.Calculation as X
import Arkham.CampaignLogKey as X
import Arkham.Card.CardDef as X
import Arkham.Card.CardType as X
import Arkham.Card.Cost as X
import Arkham.ClassSymbol as X
import Arkham.CommitRestriction as X
import Arkham.Cost as X
import Arkham.EncounterSet as X hiding (Byakhee, Dreamlands, Dunwich, Poison)
import Arkham.GameValue as X
import Arkham.Matcher as X
import Arkham.Name as X
import Arkham.Prelude as X
import Arkham.Trait as X hiding (Blight, Corruption, Evidence, Expedition, Possessed, Supply)

import Arkham.Card.CardCode (CardCode)
import Arkham.SkillType (SkillIcon)
import Arkham.SlotType (SlotType)

skills :: [SkillIcon] -> CardDef -> CardDef
skills sts def = def {cdSkills = sts}

traits :: [Trait] -> CardDef -> CardDef
traits ts def = def {cdCardTraits = setFromList ts}

slots :: [SlotType] -> CardDef -> CardDef
slots ss def = def {cdSlots = ss}

slot :: SlotType -> CardDef -> CardDef
slot s def = def {cdSlots = s : cdSlots def}

alternateCardCodes :: [CardCode] -> CardDef -> CardDef
alternateCardCodes codes def = def {cdAlternateCardCodes = codes}
