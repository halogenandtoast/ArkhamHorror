module Arkham.Location.CardDefs.NightOfTheZealot.TheGathering where

import Arkham.Location.CardDefs.Import

attic :: CardDef
attic =
  victory 1 $ location "01113" "Attic" mempty Triangle [Square] TheGathering

cellar :: CardDef
cellar =
  victory 1 $ location "01114" "Cellar" mempty Plus [Square] TheGathering

hallway :: CardDef
hallway =
  location
    "01112"
    "Hallway"
    mempty
    Square
    [Triangle, Plus, Diamond]
    TheGathering

parlor :: CardDef
parlor = location "01115" "Parlor" mempty Diamond [Square] TheGathering

study :: CardDef
study = location "01111" "Study" mempty Circle [] TheGathering
