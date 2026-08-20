module Arkham.Story.CardDefs.TheScarletKeys.DealingsInTheDark where

import Arkham.Story.CardDefs.Import

theUnsealing :: CardDef
theUnsealing = story "09571b" "The Unsealing" DealingsInTheDark & otherSideIs "09571a"

theUnveiling :: CardDef
theUnveiling = story "09571a" "The Unveiling" DealingsInTheDark & otherSideIs "09571b"
