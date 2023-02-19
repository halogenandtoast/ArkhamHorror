module Arkham.Helpers.Effect where

import Arkham.Prelude

import Arkham.Card
import Arkham.Effect.Types (Field(..))
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Projection

lookupEffectCard :: EffectId -> GameT (Maybe CardDef)
lookupEffectCard eid = do
  cardCode <- field EffectCardCode eid
  pure $ lookupCardDef cardCode
