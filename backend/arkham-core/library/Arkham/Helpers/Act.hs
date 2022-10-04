module Arkham.Helpers.Act where

import Arkham.Prelude

import Arkham.Act.Types (Field(..))
import Arkham.Act.Sequence qualified as AS
import Arkham.Id
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Matcher
import Arkham.Projection

getCurrentActStep :: (Monad m, HasGame m) => m Int
getCurrentActStep = selectJust AnyAct >>= getActStep

getActStep :: (Monad m, HasGame m) => ActId -> m Int
getActStep = fieldMap ActSequence (AS.unActStep . AS.actStep)
