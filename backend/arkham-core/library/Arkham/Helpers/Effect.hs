module Arkham.Helpers.Effect (module Arkham.Helpers.Effect, module X) where

import Arkham.Prelude

import Arkham.Effect.Window as X
import Arkham.EffectMetadata as X

import Arkham.Card
import Arkham.Effect.Types (Field (..))
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Projection

lookupEffectCard :: EffectId -> GameT (Maybe CardDef)
lookupEffectCard eid = do
  cardCode <- field EffectCardCode eid
  pure $ lookupCardDef cardCode
