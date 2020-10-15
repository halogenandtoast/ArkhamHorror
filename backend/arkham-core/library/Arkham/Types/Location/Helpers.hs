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

getGroupIsUnused
  :: (MonadReader env m, HasList UsedAbility () env) => Ability -> m Bool
getGroupIsUnused ability =
  asks $ notElem ability . map (snd . unUsedAbility) . getList ()

atLocation :: IsInvestigator investigator => investigator -> Attrs -> Bool
atLocation i Attrs { locationInvestigators } =
  getId () i `elem` locationInvestigators

getInvestigatorModifiers
  :: ( IsInvestigator investigator
     , MonadReader env m
     , HasModifiersFor env InvestigatorId env
     , MonadIO m
     )
  => investigator
  -> Attrs
  -> m [Modifier]
getInvestigatorModifiers i attrs =
  ask >>= getModifiersFor (toSource attrs) (getId @InvestigatorId () i)
