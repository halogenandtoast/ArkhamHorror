module Arkham.Helpers.Query where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Name

getLeadInvestigatorId :: (Monad m, HasGame m) => m InvestigatorId
getLeadInvestigatorId = selectJust LeadInvestigator

getActiveInvestigatorId :: (Monad m, HasGame m) => m InvestigatorId
getActiveInvestigatorId = selectJust TurnInvestigator

getInvestigatorIds :: (Monad m, HasGame m) => m [InvestigatorId]
getInvestigatorIds = selectList UneliminatedInvestigator

allInvestigatorIds :: (Monad m, HasGame m) => m [InvestigatorId]
allInvestigatorIds = selectList Anyone

selectAssetController
  :: (Monad m, HasGame m) => AssetId -> m (Maybe InvestigatorId)
selectAssetController = selectOne . HasMatchingAsset . AssetWithId

selectEventController
  :: (Monad m, HasGame m) => EventId -> m (Maybe InvestigatorId)
selectEventController = selectOne . HasMatchingEvent . EventWithId

selectSkillController
  :: (Monad m, HasGame m) => SkillId -> m (Maybe InvestigatorId)
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
getSetAsideCard :: (Monad m, HasGame m) => CardDef -> m Card
getSetAsideCard def = do
  card <- selectJust . SetAsideCardMatch $ cardIs def
  pure $ if cardCodeExactEq (toCardCode card) (toCardCode def)
    then card
    else lookupCard (toCardCode def) (toCardId card)

getSetAsideEncounterCard :: (Monad m, HasGame m) => CardDef -> m EncounterCard
getSetAsideEncounterCard =
  fmap (fromJustNote "must be encounter card" . preview _EncounterCard)
    . getSetAsideCard

getSetAsideCardsMatching :: (Monad m, HasGame m) => CardMatcher -> m [Card]
getSetAsideCardsMatching = selectList . SetAsideCardMatch

getJustLocationIdByName :: (Monad m, HasGame m) => Name -> m LocationId
getJustLocationIdByName name =
  fromJustNote ("Missing " <> show name) <$> getLocationIdByName name

getLocationIdByName :: (Monad m, HasGame m) => Name -> m (Maybe LocationId)
getLocationIdByName name = selectOne matcher
 where
  matcher = case (nameTitle name, nameSubtitle name) of
    (title, Just subtitle) -> LocationWithFullTitle title subtitle
    (title, Nothing) -> LocationWithTitle title
