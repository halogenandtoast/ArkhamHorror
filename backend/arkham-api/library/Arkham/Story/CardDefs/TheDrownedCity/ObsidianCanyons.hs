module Arkham.Story.CardDefs.TheDrownedCity.ObsidianCanyons where

import Arkham.Story.CardDefs.Import

easternWinds :: CardDef
easternWinds = story "11640b" "Eastern Winds" ObsidianCanyons & otherSideIs "11640"

erodedFriezeStory :: CardDef
erodedFriezeStory = doubleSided $ story "11664b" "Eroded Frieze" ObsidianCanyons

glyphOrreryStory :: CardDef
glyphOrreryStory = doubleSided $ story "11662b" "Glyph Orrery" ObsidianCanyons

skyRelicStory :: CardDef
skyRelicStory =
  (doubleSided $ story "11663b" "Sky Relic" ObsidianCanyons) {cdVictoryPoints = Just 1}

westernWinds :: CardDef
westernWinds = story "11640" "Western Winds" ObsidianCanyons & otherSideIs "11640b"
