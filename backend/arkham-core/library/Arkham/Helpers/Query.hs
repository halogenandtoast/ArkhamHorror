module Arkham.Helpers.Query where

import Arkham.Prelude

import Arkham.Classes.Query
import Arkham.Id
import Arkham.Matcher
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()

getLeadInvestigatorId :: (Monad m, HasGame m) => m InvestigatorId
getLeadInvestigatorId = selectJust LeadInvestigator

getActiveInvestigatorId :: (Monad m, HasGame m) => m InvestigatorId
getActiveInvestigatorId = selectJust TurnInvestigator

getInvestigatorIds :: (Monad m, HasGame m) => m [InvestigatorId]
getInvestigatorIds = selectList Anyone

selectAssetController :: (Monad m, HasGame m) => AssetId -> m (Maybe InvestigatorId)
selectAssetController = selectOne . HasMatchingAsset . AssetWithId

selectEventController :: (Monad m, HasGame m) => EventId -> m (Maybe InvestigatorId)
selectEventController = selectOne . HasMatchingEvent . EventWithId

selectSkillController :: (Monad m, HasGame m) => SkillId -> m (Maybe InvestigatorId)
selectSkillController = selectOne . HasMatchingSkill . SkillWithId

getPlayerCount :: (Monad m, HasGame m) => m Int
getPlayerCount = selectCount Anyone

-- | Get a set aside card
--
-- Some cards may be double sided and completely different types
-- like Daniel Chesterfield. In these cases, we want to consider
-- the card a match, but "flip" it to the correct side.
--
-- This logic is a bit too generous and we may want to specify
-- on double sided cards which card code is on the other side.
getSetAsideCard
  :: (Monad m, HasGame m) => CardDef
  -> m Card
getSetAsideCard def = do
  card <- selectJust . SetAsideCardMatch $ cardIs def
  pure $ if cardCodeExactEq (toCardCode card) (toCardCode def)
    then card
    else lookupCard (toCardCode def) (toCardId card)

getSetAsideEncounterCard
  :: (Monad m, HasGame m) => CardDef
  -> m EncounterCard
getSetAsideEncounterCard =
  fmap (fromJustNote "must be encounter card" . preview _EncounterCard)
    . getSetAsideCard

getSetAsideCardsMatching
  :: (Monad m, HasGame m) => CardMatcher
  -> m [Card]
getSetAsideCardsMatching = selectList . SetAsideCardMatch
