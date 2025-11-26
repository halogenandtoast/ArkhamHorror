{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaigns.TheScarletKeys.Key.Runner (module X) where

import Arkham.Calculation as X
import Arkham.Campaigns.TheScarletKeys.Key.Types as X
import Arkham.Classes as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Message as X hiding (story)
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Message.Lifted qualified as Lifted
import Arkham.Placement
import Arkham.Prelude
import Arkham.Token
import Arkham.Window qualified as Window

instance RunMessage ScarletKey where
  runMessage msg (ScarletKey a) = ScarletKey <$> runMessage msg a

instance RunMessage ScarletKeyAttrs where
  runMessage msg attrs | attrs.placement.outOfGame = runQueueT $ case msg of
    ReturnLocationToGame lid -> do
      case attrs.placement of
        OutOfGame p@(AtLocation lid') | lid' == lid -> pure $ attrs {keyPlacement = p}
        OutOfGame p@(AttachedToLocation lid') | lid' == lid -> pure $ attrs {keyPlacement = p}
        _ -> pure attrs
    _ -> pure attrs
  runMessage msg attrs = runQueueT $ case msg of
    PlaceScarletKey skid p | skid == attrs.id -> do
      case p of
        AttachedToEnemy _ | attrs.stability == Stable -> do
          pure $ attrs & placementL .~ p & stabilityL .~ Unstable
        _ -> pure $ attrs & placementL .~ p
    Flip iid _ (isTarget attrs -> True) -> do
      when (attrs.stability == Unstable) do
        Lifted.checkAfter $ Window.CampaignEvent "stabilizedKey" (Just iid) (toJSON attrs.id)
      pure
        $ attrs
        & (stabilityL .~ if attrs.stability == Stable then Unstable else Stable)
        & (shiftedL .~ True)
    EndRound -> pure $ attrs & shiftedL .~ False
    UseAbility _ ab _ | isSource attrs ab.source || isProxySource attrs ab.source -> do
      push $ Do msg
      pure attrs
    SetLocationOutOfGame lid -> do
      case attrs.placement of
        p@(AtLocation lid') | lid' == lid -> pure $ attrs {keyPlacement = OutOfGame p}
        p@(AttachedToLocation lid') | lid' == lid -> pure $ attrs {keyPlacement = OutOfGame p}
        _ -> pure attrs
    PlaceTokens _source (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _source (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    MoveTokens _ _source (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    MoveTokens _ (isSource attrs -> True) _target token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
