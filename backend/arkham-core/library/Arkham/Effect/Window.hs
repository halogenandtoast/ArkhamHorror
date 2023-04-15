module Arkham.Effect.Window
  ( module Arkham.Effect.Window
  ) where

import Arkham.Prelude
import Arkham.Card.Id

data EffectWindow
  = EffectPhaseWindow
  | EffectCostWindow
  | EffectSkillTestWindow
  | EffectRoundWindow
  | EffectSetupWindow
  | EffectTurnWindow
  | EffectCardResolutionWindow
  | EffectGameWindow
  | EffectAttackWindow
  | FirstEffectWindow [EffectWindow]
  | EffectEventWindow
  | EffectAbilityWindow
  | EffectCardCostWindow CardId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
