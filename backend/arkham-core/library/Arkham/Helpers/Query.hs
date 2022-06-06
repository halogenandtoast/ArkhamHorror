module Arkham.Helpers.Query where

import Arkham.Prelude

import Arkham.Classes.Query
import Arkham.Id
import Arkham.Matcher
import Arkham.Card

getLeadInvestigatorId :: Query InvestigatorMatcher m => m InvestigatorId
getLeadInvestigatorId = selectJust LeadInvestigator

getInvestigatorIds :: Query InvestigatorMatcher m => m [InvestigatorId]
getInvestigatorIds = selectList Anyone

selectAssetController :: Query InvestigatorMatcher m => AssetId -> m (Maybe InvestigatorId)
selectAssetController = selectOne . HasMatchingAsset . AssetWithId

selectEventController :: Query InvestigatorMatcher m => EventId -> m (Maybe InvestigatorId)
selectEventController = selectOne . HasMatchingEvent . EventWithId

selectSkillController :: Query InvestigatorMatcher m => SkillId -> m (Maybe InvestigatorId)
selectSkillController = selectOne . HasMatchingSkill . SkillWithId

getPlayerCount :: Query InvestigatorMatcher m => m Int
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
  :: (HasCallStack, Query ExtendedCardMatcher m)
  => CardDef
  -> m Card
getSetAsideCard def = do
  card <- selectJust . SetAsideCardMatch $ cardIs def
  pure $ if cardCodeExactEq (toCardCode card) (toCardCode def)
    then card
    else lookupCard (toCardCode def) (toCardId card)

getSetAsideEncounterCard
  :: (HasCallStack, Query ExtendedCardMatcher m)
  => CardDef
  -> m EncounterCard
getSetAsideEncounterCard =
  fmap (fromJustNote "must be encounter card" . preview _EncounterCard)
    . getSetAsideCard

getSetAsideCardsMatching
  :: (HasCallStack, Query ExtendedCardMatcher m)
  => CardMatcher
  -> m [Card]
getSetAsideCardsMatching = selectList . SetAsideCardMatch
