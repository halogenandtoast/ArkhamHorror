module Arkham.Helpers.Act where

import Arkham.Prelude

import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types (Field (..))
import Arkham.Card
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.Store

getCurrentActStep :: (Store m Card, HasGame m) => m Int
getCurrentActStep = selectJust AnyAct >>= getActStep

getActStep :: (HasGame m, Store m Card) => ActId -> m Int
getActStep = fieldMap ActSequence (AS.unActStep . AS.actStep)
