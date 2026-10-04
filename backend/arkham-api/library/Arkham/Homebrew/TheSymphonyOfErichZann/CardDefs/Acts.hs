{- | The Symphony of Erich Zann's act deck.

Act 1's @b@ side is not an act at all -- it flips into the Auguste Gaudin
(Conductor of the Void) enemy (see @CardDefs.Enemies.augusteGaudinConductor@),
which is why the act's own advance spawns him rather than continuing the deck.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Acts where

import Arkham.Act.CardDefs.Import
import Arkham.Homebrew.TheSymphonyOfErichZann.Sets qualified as Set

musicFromAuseilTheatre :: CardDef
musicFromAuseilTheatre =
  (act ":the-symphony-of-erich-zann:005" "Music from Auseil Theatre" 1 Set.TheSymphonyOfErichZann)
    { cdOtherSide = Just ":the-symphony-of-erich-zann:005b"
    }

thePossessedConductor :: CardDef
thePossessedConductor =
  act ":the-symphony-of-erich-zann:006" "The Possessed Conductor" 2 Set.TheSymphonyOfErichZann

undreamableOrchestra :: CardDef
undreamableOrchestra =
  act ":the-symphony-of-erich-zann:007" "Undreamable Orchestra" 3 Set.TheSymphonyOfErichZann
