module Arkham.Helpers.Location where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Location.Types ( Field (..) )
import Arkham.LocationSymbol
import Arkham.Matcher
import Arkham.Projection
import Arkham.Target

toConnections :: HasGame m => LocationId -> m [LocationSymbol]
toConnections lid =
  fieldMap LocationCard (cdLocationRevealedConnections . toCardDef) lid

getConnectedMatcher :: HasGame m => LocationId -> m LocationMatcher
getConnectedMatcher l = do
  isRevealed <- field LocationRevealed l
  directionalMatchers <- fieldMap
    LocationConnectsTo
    (map (`LocationInDirection` self) . setToList)
    l
  base <- if isRevealed
    then field LocationRevealedConnectedMatchers l
    else field LocationConnectedMatchers l

  modifiers <- getModifiers (LocationTarget l)
  LocationMatchAny
    <$> foldM applyModifier (base <> directionalMatchers) modifiers
 where
  applyModifier current (ConnectedToWhen whenMatcher matcher) = do
    matches <- member l <$> select whenMatcher
    pure $ current <> [ matcher | matches ]
  applyModifier current _ = pure current
  self = LocationWithId l
