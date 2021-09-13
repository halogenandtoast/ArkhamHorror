module Arkham.Types.Effect.Window
  ( module Arkham.Types.Effect.Window
  ) where

import Arkham.Prelude

data EffectWindow
  = EffectPhaseWindow
  | EffectSkillTestWindow
  | EffectRoundWindow
  | EffectSetupWindow
  | EffectTurnWindow
  | EffectGameWindow
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
