module Arkham.Types.Location.Helpers where

import Arkham.Import
import Arkham.Types.Location.Attrs

getIsUnused
  :: ( IsInvestigator investigator
     , HasList UsedAbility () env
     , MonadReader env m
     )
  => investigator
  -> Ability
  -> m Bool
getIsUnused i ability =
  asks $ notElem ability' . map unUsedAbility . getList ()
  where ability' = (getId () i, ability)

atLocation :: IsInvestigator investigator => investigator -> Attrs -> Bool
atLocation i Attrs { locationInvestigators } =
  getId () i `elem` locationInvestigators
