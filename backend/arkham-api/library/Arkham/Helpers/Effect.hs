module Arkham.Helpers.Effect (module Arkham.Helpers.Effect, module X) where

import Arkham.Prelude

import Arkham.Effect.Types as X (makeEffectBuilder)
import Arkham.Effect.Window as X
import Arkham.EffectMetadata as X

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Effect.Types (EffectBuilder (..), Field (..))
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Message
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Window

lookupEffectCard :: HasGame m => EffectId -> m (Maybe CardDef)
lookupEffectCard eid = do
  cardCode <- field EffectCardCode eid
  pure $ lookupCardDef cardCode

createCardEffect
  :: (Sourceable source, Targetable target)
  => CardDef
  -> Maybe (EffectMetadata Window Message)
  -> source
  -> target
  -> Message
createCardEffect def mMeta (toSource -> source) (toTarget -> target) = CreateEffect $ makeEffectBuilder (toCardCode def) mMeta source target

createMaxEffect
  :: CardDef
  -> Int
  -> EffectWindow
  -> Message
createMaxEffect def n ew =
  CreateEffect
    $ (makeEffectBuilder "maxef" (effectInt n) GameSource (CardCodeTarget $ toCardCode def))
      { effectBuilderWindow = Just ew
      }
