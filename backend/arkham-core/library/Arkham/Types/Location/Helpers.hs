module Arkham.Types.Location.Helpers
  ( module Arkham.Types.Location.Helpers
  , module X
  )
where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Game.Helpers as X

atLocation :: IsInvestigator investigator => investigator -> Attrs -> Bool
atLocation i Attrs { locationInvestigators } =
  getId () i `elem` locationInvestigators
