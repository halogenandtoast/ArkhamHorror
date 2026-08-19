module Arkham.Agenda.Cards (allAgendaCards) where

import Arkham.Agenda.CardDefEntries (allAgendaCardDefs)
import Arkham.Homebrew.Defs qualified as Homebrew

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Prelude hiding (fold)

allAgendaCards :: Map CardCode CardDef
allAgendaCards =
  (Homebrew.agendasMap <>) $ mapFromList $ map (toCardCode &&& id) allAgendaCardDefs
