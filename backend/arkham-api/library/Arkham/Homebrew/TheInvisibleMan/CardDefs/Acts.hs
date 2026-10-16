{- | Three act decks run at once. The 'a' deck is the scenario's spine; the 'c'
and 'e' decks are the two questions -- how to fight a man you cannot see, and
how to run from one -- and each pins one of Griffin's stats down when it
advances.
-}
module Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts where

import Arkham.Act.CardDefs.Import
import Arkham.Homebrew.TheInvisibleMan.Sets qualified as Set

zeroVisibility :: CardDef
zeroVisibility = act ":the-invisible-man:003" "Zero Visibility" 1 Set.TheInvisibleMan

seeThroughToTheEnd :: CardDef
seeThroughToTheEnd = act ":the-invisible-man:004" "See Through to the End" 2 Set.TheInvisibleMan

theQuestionOfFighting :: CardDef
theQuestionOfFighting = act ":the-invisible-man:005" "The Question of Fighting" 1 Set.TheInvisibleMan

fightingChance :: CardDef
fightingChance = act ":the-invisible-man:006" "Fighting Chance" 2 Set.TheInvisibleMan

theQuestionOfFleeing :: CardDef
theQuestionOfFleeing = act ":the-invisible-man:007" "The Question of Fleeing" 1 Set.TheInvisibleMan

runningInterference :: CardDef
runningInterference = act ":the-invisible-man:008" "Running Interference" 2 Set.TheInvisibleMan
