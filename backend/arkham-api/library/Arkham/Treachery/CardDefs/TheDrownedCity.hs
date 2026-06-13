module Arkham.Treachery.CardDefs.TheDrownedCity where

import Arkham.Treachery.CardDefs.Import
import Arkham.EncounterSet qualified as Set
import Arkham.Keyword qualified as Keyword

illDoItMyself :: CardDef
illDoItMyself = (weakness "11003" "\"I'll do it myself\"") {cdCardTraits = setFromList [Flaw]}

dreamsOfTheFlood :: CardDef
dreamsOfTheFlood =
  (weakness "11006" "Dreams of the Flood") {cdCardTraits = setFromList [Omen, Endtimes]}

glimpseTheVoid :: CardDef
glimpseTheVoid =
  (weakness "11010" "Glimpse the Void") {cdCardTraits = setFromList [Blunder, Insight]}

confiscation :: CardDef
confiscation = (weakness "11013" "Confiscation") {cdCardTraits = setFromList [Blunder]}

prophecyOfTheEnd :: CardDef
prophecyOfTheEnd =
  (weakness "11016" "Prophecy of the End")
    { cdCardTraits = setFromList [Omen, Endtimes]
    , cdKeywords = setFromList [Keyword.Surge]
    , cdCardType = TreacheryType
    }

castAdrift :: CardDef
castAdrift = (weakness "11019" "Cast Adrift") {cdCardTraits = setFromList [Hardship]}

downAndOut :: CardDef
downAndOut = (basicWeakness "11126" "Down and Out") {cdCardTraits = setFromList [Hardship]}

morbidCuriosity :: CardDef
morbidCuriosity = (basicWeakness "11127" "Morbid Curiosity") {cdCardTraits = setFromList [Flaw]}

disruptivePoltergeist :: CardDef
disruptivePoltergeist = (basicWeakness "11128" "Disruptive Poltergeist") {cdCardTraits = setFromList [Curse]}

frenzied :: CardDef
frenzied = (basicWeakness "11129" "Frenzied") {cdCardTraits = setFromList [Madness]}

-- One Last Job
caughtInTheCrossfire :: CardDef
caughtInTheCrossfire =
  (treachery "11515" "Caught in the Crossfire" OneLastJob 2) {cdCardTraits = setFromList [Hazard]}

endOfNegotiations :: CardDef
endOfNegotiations =
  (treachery "11516" "End of Negotiations" OneLastJob 2) {cdCardTraits = setFromList [Blunder]}

-- Dreams
torturedVisions :: CardDef
torturedVisions =
  (treachery "11749" "Tortured Visions" Dreams 3) {cdCardTraits = setFromList [Terror]}

drawnToDarkness :: CardDef
drawnToDarkness =
  (treachery "11750" "Drawn to Darkness" Dreams 3) {cdCardTraits = setFromList [Power]}

-- The Western Wall
seafloorFrieze :: CardDef
seafloorFrieze =
  (treachery "11531" "Seafloor Frieze" TheWesternWall 1)
    { cdCardTraits = setFromList [Evidence, Glyph]
    , cdOtherSide = Just "11531b"
    , cdDoubleSided = True
    }

lookOut :: CardDef
lookOut =
  (treachery "11534" "\"Look Out!\"" TheWesternWall 3) {cdCardTraits = setFromList [Terror]}

-- The Apiary
hungryWalls :: CardDef
hungryWalls =
  (treachery "11576" "Hungry Walls" TheApiary 2) {cdCardTraits = setFromList [Hazard]}

dangerousCuriosity :: CardDef
dangerousCuriosity =
  (treachery "11577" "Dangerous Curiosity" TheApiary 2) {cdCardTraits = setFromList [Blunder]}

alienEggs :: CardDef
alienEggs =
  (treachery "11578" "Alien Eggs" TheApiary 3)
    { cdCardTraits = setFromList [Hazard]
    , cdKeywords = setFromList [Keyword.Surge]
    }

parasiticTransformation :: CardDef
parasiticTransformation =
  (treachery "11583" "Parasitic Transformation" TheApiary 4)
    {cdCardTraits = setFromList [Curse, Hazard]}

-- The Grand Vault
deadlyMechanisms :: CardDef
deadlyMechanisms =
  (treachery "11607" "Deadly Mechanisms" TheGrandVault 3) {cdCardTraits = setFromList [Hazard]}

ancientVaultO :: CardDef
ancientVaultO =
  (treachery "11608" "Ancient Vault" TheGrandVault 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdOtherSide = Just "11608b"
    , cdDoubleSided = True
    }

ancientVaultN :: CardDef
ancientVaultN =
  (treachery "11609" "Ancient Vault" TheGrandVault 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdOtherSide = Just "11609b"
    , cdDoubleSided = True
    }

ancientVaultP :: CardDef
ancientVaultP =
  (treachery "11610" "Ancient Vault" TheGrandVault 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdOtherSide = Just "11610b"
    , cdDoubleSided = True
    }

-- Court of the Ancients
ancientVaultG :: CardDef
ancientVaultG =
  (treachery "11632" "Ancient Vault" CourtOfTheAncients 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdVictoryPoints = Just 1
    }

ancientVaultI :: CardDef
ancientVaultI =
  (treachery "11633" "Ancient Vault" CourtOfTheAncients 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdVictoryPoints = Just 1
    }

ruinedOrrery :: CardDef
ruinedOrrery =
  (treachery "11634" "Ruined Orrery" CourtOfTheAncients 1)
    { cdCardTraits = setFromList [Glyph]
    , cdVictoryPoints = Just 1
    }

cosmicOmen :: CardDef
cosmicOmen =
  (treachery "11636" "Cosmic Omen" CourtOfTheAncients 4)
    {cdCardTraits = setFromList [Omen, Power]}

-- Obsidian Canyons
erodedFrieze :: CardDef
erodedFrieze =
  (treachery "11664" "Eroded Frieze" ObsidianCanyons 1)
    { cdCardTraits = setFromList [Evidence, Glyph]
    , cdOtherSide = Just "11664b"
    , cdDoubleSided = True
    }

stElmosFire :: CardDef
stElmosFire =
  (treachery "11665" "St. Elmo's Fire" ObsidianCanyons 2) {cdCardTraits = setFromList [Hazard]}

acrophobia :: CardDef
acrophobia =
  (treachery "11666" "Acrophobia" ObsidianCanyons 2) {cdCardTraits = setFromList [Terror]}

wingsOfTerror :: CardDef
wingsOfTerror =
  (treachery "11667" "Wings of Terror" ObsidianCanyons 2)
    {cdCardTraits = setFromList [Scheme, Terror]}

lostInTheClouds :: CardDef
lostInTheClouds =
  (treachery "11669" "Lost in the Clouds" ObsidianCanyons 1)
    { cdCardTraits = setFromList [Blunder]
    , cdKeywords = setFromList [Keyword.Peril]
    }

-- The Doom of Arkham, Part II
layWaste :: CardDef
layWaste =
  (treachery "11716" "Lay Waste" TheDoomOfArkhamPartII 2) {cdCardTraits = setFromList [Hazard]}

eyesOfYchlecht :: CardDef
eyesOfYchlecht =
  (treachery "11717" "Eyes of Y'ch'lecht" TheDoomOfArkhamPartII 2)
    { cdCardTraits = setFromList [Power]
    , cdKeywords = setFromList [Keyword.Peril]
    }

grossPlasticity :: CardDef
grossPlasticity =
  (treachery "11719" "Gross Plasticity" TheDoomOfArkhamPartII 2)
    { cdCardTraits = setFromList [Power]
    , cdKeywords = setFromList [Keyword.Peril]
    }

arkhamUnderAssault :: CardDef
arkhamUnderAssault =
  (treachery "11720" "Arkham Under Assault" TheDoomOfArkhamPartII 2)
    {cdCardTraits = setFromList [Scheme]}

-- Stowaways
infected :: CardDef
infected =
  (treachery "11722" "Infected!" Stowaways 3) {cdCardTraits = setFromList [Hazard]}

-- Cosmic Legacy
eyeOfTheDeep :: CardDef
eyeOfTheDeep =
  (treachery "11729" "Eye of the Deep" CosmicLegacy 2) {cdCardTraits = setFromList [Power]}

cunningMimicry :: CardDef
cunningMimicry =
  (treachery "11730" "Cunning Mimicry" CosmicLegacy 2) {cdCardTraits = setFromList [Scheme]}

-- Elder Mist
elderMist :: CardDef
elderMist =
  (treachery "11731" "Elder Mist" ElderMist 2) {cdCardTraits = setFromList [Hazard]}

corrosiveFog :: CardDef
corrosiveFog =
  (treachery "11732" "Corrosive Fog" ElderMist 2) {cdCardTraits = setFromList [Hazard]}

-- Undersea Creatures
dreamingMigration :: CardDef
dreamingMigration =
  (treachery "11734" "Dreaming Migration" UnderseaCreatures 2)
    {cdCardTraits = setFromList [Hazard]}

underseaHunt :: CardDef
underseaHunt =
  (treachery "11735" "Undersea Hunt" UnderseaCreatures 2) {cdCardTraits = setFromList [Scheme]}

-- Flood
deadlyTorrent :: CardDef
deadlyTorrent =
  (treachery "11736" "Deadly Torrent" Flood 2) {cdCardTraits = setFromList [Hazard]}

somethingInTheWater :: CardDef
somethingInTheWater =
  (treachery "11737" "Something in the Water" Flood 2)
    { cdCardTraits = setFromList [Hazard]
    , cdKeywords = setFromList [Keyword.Surge]
    }

ominousSilence :: CardDef
ominousSilence =
  (treachery "11738" "Ominous Silence" Flood 2) {cdCardTraits = setFromList [Terror]}

-- R'lyeh
cyclopeanArchitecture :: CardDef
cyclopeanArchitecture =
  (treachery "11739" "Cyclopean Architecture" Set.Rlyeh 2)
    {cdCardTraits = setFromList [Terror]}

crumblingMasonry :: CardDef
crumblingMasonry =
  (treachery "11740" "Crumbling Masonry" Set.Rlyeh 2) {cdCardTraits = setFromList [Hazard]}

-- Domination
cthulhuFhtagn :: CardDef
cthulhuFhtagn =
  (treachery "11741" "\"Cthulhu fhtagn!\"" Domination 2) {cdCardTraits = setFromList [Power]}

oppressiveInfluence :: CardDef
oppressiveInfluence =
  (treachery "11742" "Oppressive Influence" Domination 2) {cdCardTraits = setFromList [Power]}

domination :: CardDef
domination =
  (treachery "11743" "Domination" Domination 1)
    { cdCardTraits = setFromList [Power]
    , cdKeywords = setFromList [Keyword.Peril]
    }

-- The Inescapable
stillBehindYou :: CardDef
stillBehindYou =
  (treachery "11745" "Still Behind You" TheInescapable 3) {cdCardTraits = setFromList [Scheme]}

-- Deep Ones
deepOneAmbush :: CardDef
deepOneAmbush =
  (treachery "11748" "Deep One Ambush" DeepOnes 2) {cdCardTraits = setFromList [Scheme]}

-- Alien Machinery
infernalMachinery :: CardDef
infernalMachinery =
  (treachery "11752" "Infernal Machinery" AlienMachinery 2)
    {cdCardTraits = setFromList [Hazard]}
