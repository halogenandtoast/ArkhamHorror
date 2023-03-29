module Arkham.Effect.Window
  ( module Arkham.Effect.Window
  ) where

import Arkham.Prelude

data EffectWindow
  = EffectPhaseWindow
  | EffectCostWindow
  | EffectSkillTestWindow
  | EffectRoundWindow
  | EffectSetupWindow
  | EffectTurnWindow
  | EffectGameWindow
  | EffectAttackWindow
  | FirstEffectWindow [EffectWindow]
  | EffectEventWindow
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
