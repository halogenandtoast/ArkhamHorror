module Arkham.Homebrew.AgainstTheWendigo.Sets (
  module Arkham.EncounterSet,
  pattern HanninahValley,
  pattern WendigosMyth,
) where

import Arkham.EncounterSet

pattern HanninahValley :: EncounterSet
pattern HanninahValley = Homebrew ":against-the-wendigo:hanninah_valley"

pattern WendigosMyth :: EncounterSet
pattern WendigosMyth = Homebrew ":against-the-wendigo:wendigos_myth"
