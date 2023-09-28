module Arkham.Helpers.Location where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.Query hiding (matches)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.LocationSymbol
import Arkham.Matcher hiding (LocationCard)
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
