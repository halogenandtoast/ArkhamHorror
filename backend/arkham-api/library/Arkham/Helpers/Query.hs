{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Helpers.Query where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.EncounterSet (EncounterSet)
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Tracing

-- TODO: IncludeEliminated is bit of a hack, if all investigators are defeated
-- there is no lead investigator so we just get someone from the eliminated. It
-- should just be the last person who was eliminated but we don't really track
-- that
getLead :: (HasCallStack, HasGame m, Tracing m) => m InvestigatorId
getLead = do
  mLead <- selectOne LeadInvestigator
  mOthers <- selectOne $ IncludeEliminated Anyone
  pure $ fromJustNote "No lead found" (mLead <|> mOthers)

getLeadMay :: (HasCallStack, HasGame m, Tracing m) => m (Maybe InvestigatorId)
getLeadMay = runMaybeT $ MaybeT (selectOne LeadInvestigator) <|> MaybeT (selectOne $ IncludeEliminated Anyone)

inTurnOrder :: HasGame m => [InvestigatorId] -> m [InvestigatorId]
inTurnOrder xs =
  getTurnOrder <&> \case
    [] -> xs -- not set yet
    ito -> filter (`elem` xs) ito

instance (Tracing m, HasGame m) => HasPlayer m where
  getPlayer = field InvestigatorPlayerId

maybeSomeInvestigator :: (HasGame m, Tracing m) => Maybe InvestigatorId -> m (Maybe InvestigatorId)
maybeSomeInvestigator (Just iid) = pure (Just iid)
maybeSomeInvestigator Nothing = selectOne ActiveInvestigator

getActiveInvestigatorId :: (HasCallStack, HasGame m, Tracing m) => m InvestigatorId
getActiveInvestigatorId = selectJust ActiveInvestigator

getInvestigatorPlayers :: (HasGame m, Tracing m) => m [(InvestigatorId, PlayerId)]
getInvestigatorPlayers = selectWithField InvestigatorPlayerId UneliminatedInvestigator

allInvestigatorPlayers :: (HasGame m, Tracing m) => m [(InvestigatorId, PlayerId)]
allInvestigatorPlayers = selectWithField InvestigatorPlayerId Anyone

getInvestigators :: (HasGame m, Tracing m) => m [InvestigatorId]
getInvestigators = inTurnOrder =<< select UneliminatedInvestigator

allInvestigators :: (HasGame m, Tracing m) => m [InvestigatorId]
allInvestigators = inTurnOrder =<< select Anyone

allPlayers :: HasGame m => m [PlayerId]
allPlayers = getAllPlayers

getLeadPlayer :: (HasCallStack, HasGame m, Tracing m) => m PlayerId
getLeadPlayer = maybe getActivePlayer (field InvestigatorPlayerId) =<< getLeadMay

getLeadInvestigatorPlayer :: (HasCallStack, HasGame m, Tracing m) => m (InvestigatorId, PlayerId)
getLeadInvestigatorPlayer = traverseToSnd (field InvestigatorPlayerId) =<< getLead

selectAssetController :: (HasGame m, Tracing m) => AssetId -> m (Maybe InvestigatorId)
selectAssetController aid =
  ((<|>) . join <$> fieldMay AssetController aid)
    <*> (join <$> fieldMay DiscardedAssetController aid)

selectEventController :: (HasGame m, Tracing m) => EventId -> m (Maybe InvestigatorId)
selectEventController = selectOne . HasMatchingEvent . EventWithId

selectSkillController :: (HasGame m, Tracing m) => SkillId -> m (Maybe InvestigatorId)
selectSkillController = selectOne . HasMatchingSkill . SkillWithId

getPlayerCount :: (HasGame m, Tracing m) => m Int
getPlayerCount = selectCount Anyone

{- | Get a set aside card

Some cards may be double sided and completely different types
like Daniel Chesterfield. In these cases, we want to consider
the card a match, but "flip" it to the correct side.

This logic is a bit too generous and we may want to specify
on double sided cards which card code is on the other side.
-}
getSetAsideCard :: (HasCallStack, HasGame m, Tracing m) => CardDef -> m Card
getSetAsideCard def = do
  card <- selectJust . SetAsideCardMatch $ cardIs def
  pure $ if exactCardCode card == exactCardCode def then card else lookupCard def.cardCode card.id

getSetAsideCardMaybe :: (HasCallStack, HasGame m, Tracing m) => CardDef -> m (Maybe Card)
getSetAsideCardMaybe def = do
  (\card -> if exactCardCode card == exactCardCode def then card else lookupCard def.cardCode card.id)
    <$$> selectOne (SetAsideCardMatch $ cardIs def)

getSetAsideEncounterCard :: (HasCallStack, HasGame m, Tracing m) => CardDef -> m EncounterCard
getSetAsideEncounterCard = fmap (fromJustNote "must be encounter card") . maybeGetSetAsideEncounterCard

getSetAsideEncounterSet :: (HasGame m, Tracing m) => EncounterSet -> m [Card]
getSetAsideEncounterSet encounterSet =
  scenarioFieldMap ScenarioSetAsideCards (filter ((== Just encounterSet) . getEncounterSet))

getOrGenerateSetAsideCard :: (CardGen m, HasGame m, Tracing m, HasCallStack) => CardDef -> m Card
getOrGenerateSetAsideCard cardDef = maybe (genCard cardDef) pure =<< maybeGetSetAsideCard cardDef

maybeGetSetAsideCard :: (HasCallStack, HasGame m, Tracing m) => CardDef -> m (Maybe Card)
maybeGetSetAsideCard def = runMaybeT do
  guardInScenario
  card <- selectMaybeT $ SetAsideCardMatch $ cardIs def
  pure $ if exactCardCode card == exactCardCode def then card else lookupCard def.cardCode card.id

maybeGetSetAsideEncounterCard :: (HasGame m, Tracing m) => CardDef -> m (Maybe EncounterCard)
maybeGetSetAsideEncounterCard def = runMaybeT do
  guardInScenario
  card <- selectMaybeT $ SetAsideCardMatch $ cardIs def
  hoistMaybe
    $ preview _EncounterCard
    $ if exactCardCode card == exactCardCode def then card else lookupCard def.cardCode card.id

getSetAsideCardsMatching :: (HasCallStack, HasGame m, Tracing m) => CardMatcher -> m [Card]
getSetAsideCardsMatching = select . SetAsideCardMatch

getJustLocationByName :: (HasCallStack, HasGame m, Tracing m) => Name -> m LocationId
getJustLocationByName name = fromJustNote ("Missing " <> show name) <$> getLocationByName name

getLocationByName :: (HasGame m, Tracing m) => Name -> m (Maybe LocationId)
getLocationByName name = selectOne matcher
 where
  matcher = case (nameTitle name, nameSubtitle name) of
    (title, Just subtitle) -> LocationWithFullTitle title subtitle
    (title, Nothing) -> LocationWithTitle title

enemiesAt
  :: (HasGame m, Tracing m, IsLocationMatcher locationMatcher) => locationMatcher -> m [EnemyId]
enemiesAt = select . EnemyAt . toLocationMatcher
