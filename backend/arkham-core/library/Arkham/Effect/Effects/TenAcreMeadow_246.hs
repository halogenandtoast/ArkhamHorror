module Arkham.Effect.Effects.TenAcreMeadow_246
  ( tenAcreMeadow_246
  , TenAcreMeadow_246(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Message

newtype TenAcreMeadow_246 = TenAcreMeadow_246 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenAcreMeadow_246 :: EffectArgs -> TenAcreMeadow_246
tenAcreMeadow_246 = TenAcreMeadow_246 . uncurry4 (baseAttrs "02246")

instance RunMessage TenAcreMeadow_246 where
  runMessage msg e@(TenAcreMeadow_246 attrs) = case msg of
    EndRound ->
      e <$ pushAll
        [RemoveClues (effectTarget attrs) 1, DisableEffect (toId attrs)]
    _ -> TenAcreMeadow_246 <$> runMessage msg attrs
