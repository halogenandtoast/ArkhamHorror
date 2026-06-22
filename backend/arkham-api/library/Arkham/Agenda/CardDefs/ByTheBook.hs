module Arkham.Agenda.CardDefs.ByTheBook where

import Arkham.Agenda.CardDefs.Import

aCovertConspiracy :: CardDef
aCovertConspiracy =
  (agenda "90033a" "A Covert Conspiracy" 1 ByTheBook) {cdOtherSide = Just "90033b"}

yourDeadlineNears :: CardDef
yourDeadlineNears = agenda "90034" "Your Deadline Nears" 2 ByTheBook
