module Arkham.Effect.Effects.SleightOfHand (
  SleightOfHand (..),
  sleightOfHand,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Matcher

newtype SleightOfHand = SleightOfHand EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

sleightOfHand :: EffectArgs -> SleightOfHand
sleightOfHand = SleightOfHand . uncurry4 (baseAttrs "03029")

instance RunMessage SleightOfHand where
  runMessage msg e@(SleightOfHand attrs) = case msg of
    EndTurn _ -> do
      case attrs.target of
        CardIdTarget cid -> do
          mAid <- selectOne (AssetWithCardId cid)
          for_ mAid \aid -> do
            mController <- selectAssetController aid
            for_ mController \controllerId ->
              push (ReturnToHand controllerId (AssetTarget aid))
        _ -> pure ()
      push $ disable attrs
      pure e
    _ -> SleightOfHand <$> runMessage msg attrs
