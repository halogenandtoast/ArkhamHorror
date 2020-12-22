module Arkham.Types.Effect.Helpers
  ( module Arkham.Types.Effect.Helpers
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Modifier

modifier :: Attrs -> ModifierType -> Modifier
modifier = Modifier . toSource

modifiers :: Attrs -> [ModifierType] -> [Modifier]
modifiers = map . modifier
