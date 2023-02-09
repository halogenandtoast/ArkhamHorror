{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Treachery.Runner
  ( module X
  ) where

import Arkham.Prelude

import Arkham.Treachery.Types as X
import Arkham.Placement as X
import Arkham.Helpers.Message as X

import Arkham.Classes.Entity
import Arkham.Classes.HasQueue
import Arkham.Classes.RunMessage
import Arkham.Message
import Arkham.Source
import Arkham.Target

instance RunMessage TreacheryAttrs where
  runMessage msg a@TreacheryAttrs {..} = case msg of
    InvestigatorEliminated iid
      | InvestigatorTarget iid `elem` treacheryAttachedTarget a -> a
      <$ push (Discard GameSource $ toTarget a)
    InvestigatorEliminated iid | Just iid == treacheryOwner ->
      a <$ push (Discard GameSource $ toTarget a)
    PlaceTreachery tid placement | tid == treacheryId ->
      pure $ a & placementL .~ placement
    PlaceResources target n | isTarget a target -> do
      pure $ a & resourcesL +~ n
    PlaceEnemyInVoid eid | EnemyTarget eid `elem` treacheryAttachedTarget a ->
      a <$ push (Discard GameSource $ toTarget a)
    AddTreacheryToHand iid tid | tid == treacheryId ->
      pure $ a & placementL .~ TreacheryInHandOf iid
    Discarded target _ | target `elem` treacheryAttachedTarget a ->
      a <$ push (Discard GameSource $ toTarget a)
    After (Revelation _ source) | isSource a source -> a <$ when
      (treacheryPlacement == TreacheryLimbo)
      (push $ Discard GameSource $ toTarget a)
    _ -> pure a
