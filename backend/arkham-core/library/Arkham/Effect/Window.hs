module Arkham.Effect.Window
  ( module Arkham.Effect.Window
  ) where

import Arkham.Prelude

data EffectWindow
  = EffectPhaseWindow
  | EffectSkillTestWindow
  | EffectRoundWindow
  | EffectSetupWindow
  | EffectTurnWindow
  | EffectGameWindow
  | FirstEffectWindow [EffectWindow]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
