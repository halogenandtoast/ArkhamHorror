module Arkham.Helpers.Effect (module Arkham.Helpers.Effect, module X) where

import Arkham.Prelude

import Arkham.Effect.Types as X (makeEffectBuilder)
import Arkham.Effect.Window as X
import Arkham.EffectMetadata as X

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Effect.Types (EffectBuilder (..), Field (..))
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Ref
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
  :: (Sourceable source, Targetable target, HasGame m)
  => CardDef
  -> Maybe (EffectMetadata Window Message)
  -> source
  -> target
  -> m Message
createCardEffect def mMeta (toSource -> source) (toTarget -> target) = do
  mcard <- sourceToMaybeCard source
  builder <- makeEffectBuilder (toCardCode def) mMeta source target
  pure $ CreateEffect $ builder {effectBuilderCardId = toCardId <$> mcard}

createMaxEffect
  :: HasGame m
  => CardDef
  -> Int
  -> EffectWindow
  -> m Message
createMaxEffect def n ew = do
  builder <- makeEffectBuilder "maxef" (effectInt n) GameSource (CardCodeTarget $ toCardCode def)
  pure $ CreateEffect $ builder {effectBuilderWindow = Just ew}
