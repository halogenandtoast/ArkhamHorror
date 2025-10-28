module Arkham.Helpers.GameLog where

import Arkham.Classes.GameLogger
import Arkham.I18n
import Arkham.Prelude

sendI18n :: (HasI18n, HasGameLogger m) => Scope -> m ()
sendI18n s = send $ ikey' s
