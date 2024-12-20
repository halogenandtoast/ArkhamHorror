module Arkham.Helpers.Text (module X, module Arkham.Helpers.Text) where

import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Text as X

import Arkham.I18n

validateEntry :: HasI18n => Bool -> Scope -> FlavorTextEntry
validateEntry cond = modifyEntry (if cond then ValidEntry else InvalidEntry) . i18nEntry

modifyEntry :: FlavorTextModifier -> FlavorTextEntry -> FlavorTextEntry
modifyEntry ftmod = \case
  ModifyEntry ftmods inner -> ModifyEntry (ftmod : ftmods) inner
  other -> ModifyEntry [ftmod] other

rightAlign :: FlavorTextEntry -> FlavorTextEntry
rightAlign = modifyEntry RightAligned

blueFlavor :: FlavorTextEntry -> FlavorText
blueFlavor = FlavorText Nothing . pure . modifyEntry BlueEntry

blueStory :: ReverseQueue m => FlavorTextEntry -> m ()
blueStory = story . blueFlavor
