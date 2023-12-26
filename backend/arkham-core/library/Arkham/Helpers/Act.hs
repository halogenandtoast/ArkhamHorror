module Arkham.Helpers.Act where

import Arkham.Prelude

import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types (Field (..))
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Source

getCurrentActStep :: HasGame m => m Int
getCurrentActStep = selectJust AnyAct >>= getActStep

getActStep :: HasGame m => ActId -> m Int
getActStep = fieldMap ActSequence (AS.unActStep . AS.actStep)

advanceVia
  :: (EntityId a ~ ActId, Sourceable source, Entity a) => AdvancementMethod -> a -> source -> Message
advanceVia method (toId -> actId) (toSource -> source) = AdvanceAct actId source method

getCurrentAct :: HasGame m => m ActId
getCurrentAct = selectOnlyOne AnyAct
