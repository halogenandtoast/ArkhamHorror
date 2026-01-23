module Arkham.Helpers.Act where

import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types (Field (..))
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Tracing

getCurrentActStep :: (Tracing m, HasGame m) => m Int
getCurrentActStep = selectJust AnyAct >>= getActStep

getActStep :: (HasGame m, Tracing m) => ActId -> m Int
getActStep = fieldMap ActSequence (AS.unActStep . AS.actStep)

advanceVia
  :: (EntityId a ~ ActId, Sourceable source, Entity a) => AdvancementMethod -> a -> source -> Message
advanceVia method (toId -> actId) (toSource -> source) = AdvanceAct actId source method

getCurrentAct :: (Tracing m, HasGame m) => m ActId
getCurrentAct = selectOnlyOne AnyAct

actMatches :: (Tracing m, HasGame m) => ActId -> Matcher.ActMatcher -> m Bool
actMatches _ Matcher.AnyAct = pure True
actMatches !actId mtchr = elem actId <$> select mtchr
