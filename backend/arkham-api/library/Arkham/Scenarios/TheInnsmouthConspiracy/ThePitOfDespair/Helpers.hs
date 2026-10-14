module Arkham.Scenarios.TheInnsmouthConspiracy.ThePitOfDespair.Helpers (module X, module Arkham.Scenarios.TheInnsmouthConspiracy.ThePitOfDespair.Helpers) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers as X
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Narrative
import Arkham.I18n
import Arkham.Message.Lifted
import Arkham.Narrative.TheInnsmouthConspiracy
import Arkham.Prelude
import Arkham.Text

data Flashback = Flashback1 | Flashback2 | Flashback3 | Flashback4

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ scope "theInnsmouthConspiracy" $ scope "thePitOfDespair" a

flashback :: ReverseQueue m => Flashback -> m ()
flashback f = scenarioI18n case f of
  Flashback1 -> do
    narrative <- getInnsmouthNarrative
    storyBuild
      =<< narrativeBuilder
        narrative.thePitOfDespairFlashback1
        (setTitle "flashback1.title" >> p "flashback1.body")
  Flashback2 -> story $ i18nWithTitle "flashback2"
  Flashback3 -> story $ i18nWithTitle "flashback3"
  Flashback4 -> story $ i18nWithTitle "flashback4"
