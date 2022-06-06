module Arkham.Helpers.Query where

import Arkham.Prelude

import Arkham.Classes.Query
import Arkham.Id
import Arkham.Matcher
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()

getLeadInvestigatorId :: GameT InvestigatorId
getLeadInvestigatorId = selectJust LeadInvestigator

getActiveInvestigatorId :: GameT InvestigatorId
getActiveInvestigatorId = selectJust TurnInvestigator

getInvestigatorIds :: GameT [InvestigatorId]
getInvestigatorIds = selectList Anyone

selectAssetController :: AssetId -> GameT (Maybe InvestigatorId)
selectAssetController = selectOne . HasMatchingAsset . AssetWithId

selectEventController :: EventId -> GameT (Maybe InvestigatorId)
selectEventController = selectOne . HasMatchingEvent . EventWithId

selectSkillController :: SkillId -> GameT (Maybe InvestigatorId)
selectSkillController = selectOne . HasMatchingSkill . SkillWithId

getPlayerCount :: GameT Int
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
  :: CardDef
  -> GameT Card
getSetAsideCard def = do
  card <- selectJust . SetAsideCardMatch $ cardIs def
  pure $ if cardCodeExactEq (toCardCode card) (toCardCode def)
    then card
    else lookupCard (toCardCode def) (toCardId card)

getSetAsideEncounterCard
  :: CardDef
  -> GameT EncounterCard
getSetAsideEncounterCard =
  fmap (fromJustNote "must be encounter card" . preview _EncounterCard)
    . getSetAsideCard

getSetAsideCardsMatching
  :: CardMatcher
  -> GameT [Card]
getSetAsideCardsMatching = selectList . SetAsideCardMatch
