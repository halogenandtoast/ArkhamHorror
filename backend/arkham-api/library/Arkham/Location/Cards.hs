module Arkham.Location.Cards where

import Arkham.Homebrew.Defs qualified as Homebrew
import Arkham.Location.CardDefEntries (allLocationCardDefs)
import Arkham.Location.CardDefs.Import

allLocationCards :: Map CardCode CardDef
allLocationCards =
  (Homebrew.locationsMap <>) $ mapFromList $ map (toCardCode &&& id) allLocationCardDefs

allSpecialLocationCards :: Map CardCode CardDef
allSpecialLocationCards =
  mapFromList $ map (toCardCode &&& id) [betweenWorlds, emptySpace, openSky]

betweenWorlds :: CardDef
betweenWorlds =
  location "xbetween" "Between Worlds" [Otherworld] NoSymbol [] ShatteredAeons

emptySpace :: CardDef
emptySpace =
  location "xempty" "EmptySpace" [] NoSymbol [] BeforeTheBlackThrone

{- | Obsidian Canyons' "open sky". The printed open sky cards are facedown
Ancient Evils / Chilling Cold / Striking Fear encounter cards, whose only
mechanical role is to be absent from the encounter deck; setup removes those and
puts one of these in each open sky position instead, so the grid, the Summit
deck, and every "swap with an adjacent open sky" effect see a uniform location.
-}
openSky :: CardDef
openSky =
  location "xsky" "Open Sky" [] NoSymbol [] ObsidianCanyons
