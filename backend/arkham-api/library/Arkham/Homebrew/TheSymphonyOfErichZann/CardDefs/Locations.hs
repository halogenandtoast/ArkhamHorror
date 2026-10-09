{- | The Auseil Theatre.

The four front-of-house rooms start in play. The six Backstage Rooms are set
aside and only unlocked when act 2 advances: four are put into play at random
and the other two are removed from the game, so every one of them prints the
same unrevealed face ("Backstage Room", Moon, connecting to the Diamond) and
differs only once revealed.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations where

import Arkham.Homebrew.TheSymphonyOfErichZann.Sets qualified as Set
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits hiding (pattern Piano)
import Arkham.Location.CardDefs.Import

-- Front of house: in play from setup.

entranceHall :: CardDef
entranceHall =
  location
    ":the-symphony-of-erich-zann:009"
    "Entrance Hall"
    [AuseilTheatre]
    Circle
    [Square]
    Set.TheSymphonyOfErichZann

mainLobby :: CardDef
mainLobby =
  location
    ":the-symphony-of-erich-zann:010"
    "Main Lobby"
    [AuseilTheatre]
    Square
    [Circle, Triangle, Plus, Diamond]
    Set.TheSymphonyOfErichZann

gallery :: CardDef
gallery =
  location
    ":the-symphony-of-erich-zann:011"
    "Gallery"
    [AuseilTheatre]
    Triangle
    [Square, Plus]
    Set.TheSymphonyOfErichZann

auditorium :: CardDef
auditorium =
  location
    ":the-symphony-of-erich-zann:012"
    "Auditorium"
    [AuseilTheatre]
    Plus
    [Square, Triangle, Diamond]
    Set.TheSymphonyOfErichZann

-- | The six Backstage Rooms. Identical unrevealed face, one shared back.
backstageRoom :: CardCode -> Name -> CardDef
backstageRoom cardCode name =
  locationWithUnrevealed
    cardCode
    "Backstage Room"
    [AuseilTheatre, Backstage]
    Moon
    [Diamond]
    name
    [AuseilTheatre, Backstage]
    Moon
    [Diamond]
    Set.TheSymphonyOfErichZann

anechoicChamber :: CardDef
anechoicChamber = backstageRoom ":the-symphony-of-erich-zann:013" "Anechoic Chamber"

instrumentCloset :: CardDef
instrumentCloset = backstageRoom ":the-symphony-of-erich-zann:014" "Instrument Closet"

recordingStudio :: CardDef
recordingStudio = backstageRoom ":the-symphony-of-erich-zann:015" "Recording Studio"

rehearsalRoom :: CardDef
rehearsalRoom = backstageRoom ":the-symphony-of-erich-zann:016" "Rehearsal Room"

sceneShop :: CardDef
sceneShop = backstageRoom ":the-symphony-of-erich-zann:017" "Scene Shop"

tiringRoom :: CardDef
tiringRoom = backstageRoom ":the-symphony-of-erich-zann:018" "Tiring Room"

stageHall :: CardDef
stageHall =
  location
    ":the-symphony-of-erich-zann:019"
    "Stage Hall"
    [AuseilTheatre, Backstage]
    Diamond
    [Square, Plus, Moon]
    Set.TheSymphonyOfErichZann
