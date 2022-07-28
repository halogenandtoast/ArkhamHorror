module Arkham.Helpers.Location where

import Arkham.Prelude

import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Location.Types ( Field (..) )
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.Source
import Arkham.Target

getConnectedMatcher :: (Monad m, HasGame m) => LocationId -> m LocationMatcher
getConnectedMatcher l = do
  isRevealed <- field LocationRevealed l
  directionalMatchers <- fieldMap
    LocationConnectsTo
    (map (`LocationInDirection` self) . setToList)
    l
  base <- if isRevealed
    then field LocationRevealedConnectedMatchers l
    else field LocationConnectedMatchers l

  modifiers <- getModifiers (LocationSource l) (LocationTarget l)
  LocationMatchAny
    <$> foldM applyModifier (base <> directionalMatchers) modifiers
 where
  applyModifier current (ConnectedToWhen whenMatcher matcher) = do
    matches <- member l <$> select whenMatcher
    pure $ current <> [ matcher | matches ]
  applyModifier current _ = pure current
  self = LocationWithId l
