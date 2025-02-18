module Arkham.Helpers.UI (module Arkham.Helpers.UI, module X) where

import Arkham.Classes.HasGame
import Arkham.Effect.Window
import Arkham.Message (Message)
import Arkham.Source
import Arkham.Target
import Arkham.Helpers.Modifiers
import Arkham.UI as X

uiEffect
  :: (HasGame m, Sourceable source, Targetable target) => source -> target -> UIModifier -> m Message
uiEffect source target modifier = createWindowModifierEffect EffectUI source target [UIModifier modifier]
