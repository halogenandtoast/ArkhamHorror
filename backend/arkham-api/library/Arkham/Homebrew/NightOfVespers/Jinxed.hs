{- | The Jinxed keyword's slug, split out of 'Arkham.Homebrew.NightOfVespers.Jinx'
so a CardDef can declare it: Jinx pulls in 'Arkham.Message', which the CardDefs
modules are upstream of.
-}
module Arkham.Homebrew.NightOfVespers.Jinxed (pattern Jinxed) where

import Arkham.Keyword (Keyword (CustomKeyword))

pattern Jinxed :: Keyword
pattern Jinxed = CustomKeyword "nightOfVespers.jinxed"
