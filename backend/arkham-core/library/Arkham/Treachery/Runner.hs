{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Treachery.Runner (
  module X,
  addHiddenToHand,
) where

import Arkham.Prelude

import Arkham.Helpers.Message as X
import Arkham.Helpers.SkillTest as X
import Arkham.Placement as X
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Treachery.Types as X

import Arkham.Classes.Entity
import Arkham.Classes.HasQueue
import Arkham.Classes.RunMessage
import Arkham.Id
import Arkham.Message
import Arkham.Token

addHiddenToHand :: InvestigatorId -> TreacheryAttrs -> Message
addHiddenToHand iid a = PlaceTreachery (toId a) (TreacheryInHandOf iid)

instance RunMessage TreacheryAttrs where
  runMessage msg a@TreacheryAttrs {..} = case msg of
    InvestigatorEliminated iid
      | InvestigatorTarget iid `elem` treacheryAttachedTarget a -> do
          push (Discard GameSource $ toTarget a)
          pure a
    InvestigatorEliminated iid
      | Just iid == treacheryOwner ->
          a <$ push (Discard GameSource $ toTarget a)
    PlaceTreachery tid placement
      | tid == treacheryId ->
          pure $ a & placementL .~ placement
    PlaceTokens _ (isTarget a -> True) token n -> do
      pure $ a & tokensL %~ addTokens token n
    PlaceEnemyInVoid eid
      | EnemyTarget eid `elem` treacheryAttachedTarget a ->
          a <$ push (Discard GameSource $ toTarget a)
    Discarded target _ _
      | target `elem` treacheryAttachedTarget a ->
          a <$ push (Discard GameSource $ toTarget a)
    After (Revelation _ (isSource a -> True)) -> do
      when
        (treacheryPlacement == TreacheryLimbo)
        (push $ Discard GameSource $ toTarget a)
      pure a
    _ -> pure a
