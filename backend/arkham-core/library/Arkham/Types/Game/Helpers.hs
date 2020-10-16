module Arkham.Types.Game.Helpers where

import Arkham.Import

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

getInvestigatorModifiers
  :: ( IsInvestigator investigator
     , MonadReader env m
     , HasModifiersFor env InvestigatorId env
     , MonadIO m
     )
  => investigator
  -> Source
  -> m [Modifier]
getInvestigatorModifiers i source =
  ask >>= getModifiersFor source (getId @InvestigatorId () i)

getXp :: (HasCount XPCount () env, MonadReader env m) => m Int
getXp = asks $ unXPCount . getCount ()

getLeadInvestigatorId
  :: (HasId LeadInvestigatorId () env, MonadReader env m) => m InvestigatorId
getLeadInvestigatorId = asks $ unLeadInvestigatorId . getId ()

getInvestigatorIds
  :: (HasSet InvestigatorId () env, MonadReader env m) => m [InvestigatorId]
getInvestigatorIds = asks $ setToList . getSet ()

getPlayerCount :: (HasCount PlayerCount () env, MonadReader env m) => m Int
getPlayerCount = asks $ unPlayerCount . getCount ()

getPlayerCountValue
  :: (HasCount PlayerCount () env, MonadReader env m) => GameValue Int -> m Int
getPlayerCountValue gameValue = fromGameValue gameValue <$> getPlayerCount

getLocationSet
  :: (HasSet LocationId () env, MonadReader env m) => m (HashSet LocationId)
getLocationSet = asks $ getSet ()

getSpendableClueCount
  :: (MonadReader env m, HasCount SpendableClueCount InvestigatorId env)
  => [InvestigatorId]
  -> m Int
getSpendableClueCount investigatorIds =
  sum <$> for investigatorIds (asks . (unSpendableClueCount .) . getCount)
