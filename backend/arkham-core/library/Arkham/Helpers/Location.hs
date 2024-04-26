module Arkham.Helpers.Location where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query hiding (matches)
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.LocationSymbol
import Arkham.Matcher hiding (LocationCard)
import Arkham.Placement
import Arkham.Projection
import Arkham.Target

toConnections :: HasGame m => LocationId -> m [LocationSymbol]
toConnections lid =
  fieldMap LocationCard (cdLocationRevealedConnections . toCardDef) lid

getConnectedMatcher :: HasGame m => LocationId -> m LocationMatcher
getConnectedMatcher l = do
  isRevealed <- field LocationRevealed l
  directionalMatchers <-
    fieldMap
      LocationConnectsTo
      (map (`LocationInDirection` self) . setToList)
      l
  base <-
    if isRevealed
      then field LocationRevealedConnectedMatchers l
      else field LocationConnectedMatchers l

  modifiers <- getModifiers (LocationTarget l)
  LocationMatchAny
    <$> foldM applyModifier (base <> directionalMatchers) modifiers
 where
  applyModifier current (ConnectedToWhen whenMatcher matcher) = do
    matches <- elem l <$> select whenMatcher
    pure $ current <> [matcher | matches]
  applyModifier current _ = pure current
  self = LocationWithId l

isAt :: (HasGame m, Entity a, EntityId a ~ LocationId) => InvestigatorId -> a -> m Bool
isAt iid (toId -> lid) = fieldMap InvestigatorLocation (== Just lid) iid

placementLocation :: HasGame m => Placement -> m (Maybe LocationId)
placementLocation = \case
  AtLocation lid -> pure $ Just lid
  AttachedToLocation lid -> pure $ Just lid
  InPlayArea iid -> field InvestigatorLocation iid
  InThreatArea iid -> field InvestigatorLocation iid
  AttachedToInvestigator iid -> field InvestigatorLocation iid
  AttachedToEnemy eid -> field EnemyLocation eid
  AttachedToAsset aid' _ -> field AssetLocation aid'
  AttachedToAct _ -> pure Nothing
  AttachedToAgenda _ -> pure Nothing
  Unplaced -> pure Nothing
  Global -> pure Nothing
  Limbo -> pure Nothing
  OutOfPlay _ -> pure Nothing
  StillInHand _ -> pure Nothing
  StillInDiscard _ -> pure Nothing
  StillInEncounterDiscard -> pure Nothing
  AsSwarm eid _ -> field EnemyLocation eid
