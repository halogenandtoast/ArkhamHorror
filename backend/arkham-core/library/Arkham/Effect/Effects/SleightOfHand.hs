module Arkham.Effect.Effects.SleightOfHand
  ( SleightOfHand(..)
  , sleightOfHand
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype SleightOfHand = SleightOfHand EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHand :: EffectArgs -> SleightOfHand
sleightOfHand = SleightOfHand . uncurry4 (baseAttrs "03029")

instance RunMessage SleightOfHand where
  runMessage msg e@(SleightOfHand attrs) = case msg of
    EndTurn _ -> do
      case effectTarget attrs of
        AssetTarget aid -> do
          inPlay <- isJust <$> selectOne (AssetWithId aid)
          when inPlay $ do
            mController <- selectAssetController aid
            for_ mController $ \controllerId ->
              push (ReturnToHand controllerId (AssetTarget aid))
        _ -> pure ()
      push $ Discard (toSource attrs) (toTarget attrs)
      pure e
    _ -> SleightOfHand <$> runMessage msg attrs
