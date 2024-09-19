module Arkham.Scenarios.ThePitOfDespair.Helpers (module X, module Arkham.Scenarios.ThePitOfDespair.Helpers) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers as X
import Arkham.I18n
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Text

data Flashback = Flashback1 | Flashback2 | Flashback3 | Flashback4

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ scope "theInnsmouthConspiracy" $ scope "thePitOfDespair" a

flashback :: ReverseQueue m => Flashback -> m ()
flashback f = scenarioI18n $ story $ i18nWithTitle fkey
 where
  fkey = case f of
    Flashback1 -> "flashback1"
    Flashback2 -> "flashback2"
    Flashback3 -> "flashback3"
    Flashback4 -> "flashback4"
