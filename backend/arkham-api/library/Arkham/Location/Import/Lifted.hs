module Arkham.Location.Import.Lifted (module X, module Arkham.Location.Import.Lifted) where

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.GameValue as X
import Arkham.Helpers.Message as X (pattern R1, pattern R2)
import Arkham.Helpers.Modifiers as X (toModifiers)
import Arkham.Location.Helpers as X (adjacentLocations)
import Arkham.Location.Runner as X (
  IsLocation,
  LocationAttrs (..),
  LocationCard,
  Message (..),
  canBeFlippedL,
  connectedMatchersL,
  connectsToL,
  costToEnterUnrevealedL,
  extendRevealed,
  extendRevealed1,
  floodLevelL,
  getLeadPlayer,
  getLocationMetaDefault,
  getSetAsideCard,
  globalMetaL,
  is,
  labelL,
  location,
  locationResignAction,
  locationWith,
  push,
  pushAll,
  revealedConnectedMatchersL,
  setMeta,
  symbolLabel,
  veiled,
  veiled1,
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern PassedThisSkillTest,
  pattern PassedThisSkillTestBy,
  pattern PlaceDoom,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Classes.HasGame
import Arkham.Helpers.Modifiers

whenRevealed :: (HasGame m, Monoid x) => LocationAttrs -> m x -> m x
whenRevealed attrs body = if attrs.revealed then body else pure mempty

whenUnrevealed :: (HasGame m, Monoid x) => LocationAttrs -> m x -> m x
whenUnrevealed attrs body = if attrs.unrevealed then body else pure mempty

blockedWhen :: HasGame m => LocationAttrs -> m Bool -> m (Map Target [Modifier])
blockedWhen attrs body = do
  cond <- body
  if cond then modifySelf attrs [Blocked] else pure mempty

blockedUnless :: HasGame m => LocationAttrs -> m Bool -> m (Map Target [Modifier])
blockedUnless attrs body = blockedWhen attrs (not <$> body)

blockedWhenAny :: (Query query, HasGame m) => LocationAttrs -> query -> m (Map Target [Modifier])
blockedWhenAny attrs query = blockedWhen attrs (selectAny query)
