module Arkham.Helpers.Query where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.EncounterSet (EncounterSet)
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))

-- TODO: IncludeEliminated is bit of a hack, if all investigators are defeated
-- there is no lead investigator so we just get someone from the eliminated. It
-- should just be the last person who was eliminated but we don't really track
-- that
getLeadInvestigatorId :: (HasCallStack, HasGame m) => m InvestigatorId
getLeadInvestigatorId = do
  mLead <- selectOne LeadInvestigator
  mOthers <- selectOne (IncludeEliminated Anyone)
  pure $ fromJustNote "No lead found" (mLead <|> mOthers)

getLead :: HasGame m => m InvestigatorId
getLead = getLeadInvestigatorId

getPlayer :: HasGame m => InvestigatorId -> m PlayerId
getPlayer = field InvestigatorPlayerId

getActiveInvestigatorId :: HasGame m => m InvestigatorId
getActiveInvestigatorId = selectJust ActiveInvestigator

getInvestigatorPlayers :: HasGame m => m [(InvestigatorId, PlayerId)]
getInvestigatorPlayers = selectWithField InvestigatorPlayerId UneliminatedInvestigator

getAllInvestigatorPlayers :: HasGame m => m [(InvestigatorId, PlayerId)]
getAllInvestigatorPlayers = selectWithField InvestigatorPlayerId Anyone

allInvestigatorPlayers :: HasGame m => m [(InvestigatorId, PlayerId)]
allInvestigatorPlayers = getAllInvestigatorPlayers

getInvestigators :: HasGame m => m [InvestigatorId]
getInvestigators = getInvestigatorIds

getInvestigatorIds :: HasGame m => m [InvestigatorId]
getInvestigatorIds = selectList UneliminatedInvestigator

allInvestigatorIds :: HasGame m => m [InvestigatorId]
allInvestigatorIds = selectList Anyone

allInvestigators :: HasGame m => m [InvestigatorId]
allInvestigators = allInvestigatorIds

allPlayers :: HasGame m => m [PlayerId]
allPlayers = selectFields InvestigatorPlayerId Anyone

getLeadPlayer :: HasGame m => m PlayerId
getLeadPlayer = field InvestigatorPlayerId =<< getLead

getLeadInvestigatorPlayer :: HasGame m => m (InvestigatorId, PlayerId)
getLeadInvestigatorPlayer = traverseToSnd (field InvestigatorPlayerId) =<< getLead

selectAssetController
  :: HasGame m => AssetId -> m (Maybe InvestigatorId)
selectAssetController = selectOne . HasMatchingAsset . AssetWithId

selectEventController
  :: HasGame m => EventId -> m (Maybe InvestigatorId)
selectEventController = selectOne . HasMatchingEvent . EventWithId

selectSkillController
  :: HasGame m => SkillId -> m (Maybe InvestigatorId)
selectSkillController = selectOne . HasMatchingSkill . SkillWithId

getPlayerCount :: HasGame m => m Int
getPlayerCount = selectCount Anyone

{- | Get a set aside card

Some cards may be double sided and completely different types
like Daniel Chesterfield. In these cases, we want to consider
the card a match, but "flip" it to the correct side.

This logic is a bit too generous and we may want to specify
on double sided cards which card code is on the other side.
-}
getSetAsideCard :: HasGame m => CardDef -> m Card
getSetAsideCard def = do
  card <- selectJust . SetAsideCardMatch $ cardIs def
  pure
    $ if cardCodeExactEq (toCardCode card) (toCardCode def)
      then card
      else lookupCard (toCardCode def) (toCardId card)

getSetAsideEncounterCard :: HasGame m => CardDef -> m EncounterCard
getSetAsideEncounterCard =
  fmap (fromJustNote "must be encounter card") . maybeGetSetAsideEncounterCard

getSetAsideEncounterSet :: HasGame m => EncounterSet -> m [Card]
getSetAsideEncounterSet encounterSet =
  scenarioFieldMap
    ScenarioSetAsideCards
    (filter ((== Just encounterSet) . cdEncounterSet . toCardDef))

maybeGetSetAsideEncounterCard :: HasGame m => CardDef -> m (Maybe EncounterCard)
maybeGetSetAsideEncounterCard = fmap (preview _EncounterCard) . getSetAsideCard

getSetAsideCardsMatching :: HasGame m => CardMatcher -> m [Card]
getSetAsideCardsMatching = selectList . SetAsideCardMatch

getJustLocationByName :: HasGame m => Name -> m LocationId
getJustLocationByName name =
  fromJustNote ("Missing " <> show name) <$> getLocationByName name

getLocationByName :: HasGame m => Name -> m (Maybe LocationId)
getLocationByName name = selectOne matcher
 where
  matcher = case (nameTitle name, nameSubtitle name) of
    (title, Just subtitle) -> LocationWithFullTitle title subtitle
    (title, Nothing) -> LocationWithTitle title

enemiesAt :: (HasGame m, IsLocationMatcher locationMatcher) => locationMatcher -> m [EnemyId]
enemiesAt = selectList . EnemyAt . toLocationMatcher
