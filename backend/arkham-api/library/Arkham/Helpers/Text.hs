module Arkham.Helpers.Text (module X, module Arkham.Helpers.Text) where

import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Text as X

validateEntry :: Bool -> FlavorTextEntry -> FlavorTextEntry
validateEntry True = \case
  ModifyEntry xs body -> ModifyEntry (ValidEntry : xs) body
  body -> ModifyEntry [ValidEntry] body
validateEntry False = \case
  ModifyEntry xs body -> ModifyEntry (InvalidEntry : xs) body
  body -> ModifyEntry [InvalidEntry] body

rightAlign :: FlavorTextEntry -> FlavorTextEntry
rightAlign = \case
  ModifyEntry xs body -> ModifyEntry (RightAligned : xs) body
  body -> ModifyEntry [RightAligned] body

blueFlavor :: FlavorTextEntry -> FlavorText
blueFlavor entry = FlavorText Nothing [ModifyEntry [BlueEntry] entry]

blueStory :: ReverseQueue m => FlavorTextEntry -> m ()
blueStory = story . blueFlavor
