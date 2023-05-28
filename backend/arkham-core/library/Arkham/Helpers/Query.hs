module Arkham.Helpers.Query where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Query
import Arkham.EncounterSet (EncounterSet)
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.Scenario.Types (Field (..))
import Arkham.Store

-- Anyone is bit of a hack, if all investigators are defeated there is not lead
-- investigator so we use the `Anyone` scope to bring in all eliminated
-- investigators as well, for which the last should be the lead.
getLeadInvestigatorId :: (HasGame m, Store m Card) => m InvestigatorId
getLeadInvestigatorId = selectJust $ Anyone <> LeadInvestigator

getLead :: (HasGame m, Store m Card) => m InvestigatorId
getLead = getLeadInvestigatorId

getActiveInvestigatorId :: (HasGame m, Store m Card) => m InvestigatorId
getActiveInvestigatorId = selectJust ActiveInvestigator

getInvestigatorIds :: (HasGame m, Store m Card) => m [InvestigatorId]
getInvestigatorIds = selectList UneliminatedInvestigator

allInvestigatorIds :: (HasGame m, Store m Card) => m [InvestigatorId]
allInvestigatorIds = selectList Anyone

selectAssetController
  :: (HasGame m, Store m Card) => AssetId -> m (Maybe InvestigatorId)
selectAssetController = selectOne . HasMatchingAsset . AssetWithId

selectEventController
  :: (HasGame m, Store m Card) => EventId -> m (Maybe InvestigatorId)
selectEventController = selectOne . HasMatchingEvent . EventWithId

selectSkillController
  :: (HasGame m, Store m Card) => SkillId -> m (Maybe InvestigatorId)
selectSkillController = selectOne . HasMatchingSkill . SkillWithId

getPlayerCount :: (HasGame m, Store m Card) => m Int
getPlayerCount = selectCount Anyone

{- | Get a set aside card

Some cards may be double sided and completely different types
like Daniel Chesterfield. In these cases, we want to consider
the card a match, but "flip" it to the correct side.

This logic is a bit too generous and we may want to specify
on double sided cards which card code is on the other side.
-}
getSetAsideCard :: (HasGame m, Store m Card) => CardDef -> m Card
getSetAsideCard def = do
  card <- selectJust . SetAsideCardMatch $ cardIs def
  pure $
    if cardCodeExactEq (toCardCode card) (toCardCode def)
      then card
      else lookupCard (toCardCode def) (toCardId card)

getSetAsideEncounterCard :: (HasGame m, Store m Card) => CardDef -> m EncounterCard
getSetAsideEncounterCard =
  fmap (fromJustNote "must be encounter card") . maybeGetSetAsideEncounterCard

getSetAsideEncounterSet :: (HasGame m, Store m Card) => EncounterSet -> m [Card]
getSetAsideEncounterSet encounterSet =
  scenarioFieldMap
    ScenarioSetAsideCards
    (filter ((== Just encounterSet) . cdEncounterSet . toCardDef))

maybeGetSetAsideEncounterCard :: (HasGame m, Store m Card) => CardDef -> m (Maybe EncounterCard)
maybeGetSetAsideEncounterCard = fmap (preview _EncounterCard) . getSetAsideCard

getSetAsideCardsMatching :: (HasGame m, Store m Card) => CardMatcher -> m [Card]
getSetAsideCardsMatching = selectList . SetAsideCardMatch

getJustLocationIdByName :: (HasGame m, Store m Card) => Name -> m LocationId
getJustLocationIdByName name =
  fromJustNote ("Missing " <> show name) <$> getLocationIdByName name

getLocationIdByName :: (HasGame m, Store m Card) => Name -> m (Maybe LocationId)
getLocationIdByName name = selectOne matcher
 where
  matcher = case (nameTitle name, nameSubtitle name) of
    (title, Just subtitle) -> LocationWithFullTitle title subtitle
    (title, Nothing) -> LocationWithTitle title

enemiesAt
  :: (HasGame m, IsLocationMatcher locationMatcher, Store m Card) => locationMatcher -> m [EnemyId]
enemiesAt = selectList . EnemyAt . toLocationMatcher
