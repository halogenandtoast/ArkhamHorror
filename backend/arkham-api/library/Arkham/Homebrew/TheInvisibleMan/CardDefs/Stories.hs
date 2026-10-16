{- | The Invisible Combat and Invisible Escape decks. Each card is drawn, read
and shuffled back as Griffin's answer to an attack or an evasion; the trait is
what sorts them into their two decks at setup.
-}
module Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Homebrew.TheInvisibleMan.Sets qualified as Set
import Arkham.Homebrew.TheInvisibleMan.Traits
import Arkham.Name
import Arkham.Prelude
import Arkham.Story.CardDefs.Base

combat :: CardCode -> Name -> CardDef
combat cardCode name =
  (story cardCode name Set.TheInvisibleMan) {cdCardTraits = singleton InvisibleCombat}

escape :: CardCode -> Name -> CardDef
escape cardCode name =
  (story cardCode name Set.TheInvisibleMan) {cdCardTraits = singleton InvisibleEscape}

everywhereAndNowhere :: CardDef
everywhereAndNowhere = combat ":the-invisible-man:017" "Everywhere and Nowhere"

graspingAtStraws :: CardDef
graspingAtStraws = combat ":the-invisible-man:018" "Grasping at Straws"

shotInTheDark :: CardDef
shotInTheDark = combat ":the-invisible-man:019" "Shot in the Dark"

patternRecognition :: CardDef
patternRecognition = combat ":the-invisible-man:020" "Pattern Recognition"

subtleSigns :: CardDef
subtleSigns = combat ":the-invisible-man:021" "Subtle Signs"

gotHisNumber :: CardDef
gotHisNumber = combat ":the-invisible-man:022" "Got His Number"

nowhereToRun :: CardDef
nowhereToRun = escape ":the-invisible-man:023" "Nowhere to Run"

panickedFlight :: CardDef
panickedFlight = escape ":the-invisible-man:024" "Panicked Flight"

shadowedEscape :: CardDef
shadowedEscape = escape ":the-invisible-man:025" "Shadowed Escape"

sharpEarOut :: CardDef
sharpEarOut = escape ":the-invisible-man:026" "Sharp Ear Out"
