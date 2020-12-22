module Arkham.Types.Event.Helpers
  ( module Arkham.Types.Event.Helpers
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Modifier

modifier :: Attrs -> ModifierType -> Modifier
modifier = Modifier . toSource

modifiers :: Attrs -> [ModifierType] -> [Modifier]
modifiers = map . modifier
