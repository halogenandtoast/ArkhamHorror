module Arkham.Homebrew.CircusExMortis.CardDefs.Stories where

import Arkham.Card.CardDef
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Prelude
import Arkham.Story.CardDefs.Base

-- Circus Ex Mortis (fan campaign by Tyler Gotch): harm_s_way
theDarkYoungStir :: CardDef
theDarkYoungStir =
  doubleSided $ story ":circus-ex-mortis:060" "The Dark Young Stir..." Set.HarmsWay

hiddenInPlainSight :: CardDef
hiddenInPlainSight =
  doubleSided $ story ":circus-ex-mortis:061" "Hidden in Plain Sight" Set.HarmsWay

underLockAndKey :: CardDef
underLockAndKey =
  doubleSided $ story ":circus-ex-mortis:062" "Under Lock and Key" Set.HarmsWay

cautiousJailers :: CardDef
cautiousJailers =
  doubleSided $ story ":circus-ex-mortis:063" "Cautious Jailers" Set.HarmsWay

deepInTheDark :: CardDef
deepInTheDark =
  doubleSided $ story ":circus-ex-mortis:064" "Deep in the Dark" Set.HarmsWay

clappedInIrons :: CardDef
clappedInIrons =
  doubleSided $ story ":circus-ex-mortis:065" "Clapped in Irons" Set.HarmsWay

hypnoticState :: CardDef
hypnoticState =
  doubleSided $ story ":circus-ex-mortis:066" "Hypnotic State" Set.HarmsWay

-- Circus Ex Mortis (fan campaign by Tyler Gotch): red_sunrise
pathForward_180 :: CardDef
pathForward_180 =
  doubleSided $ (story ":circus-ex-mortis:180" "Path Forward" Set.RedSunrise) {cdEncounterSetQuantity = Just 2}

pathForward_181 :: CardDef
pathForward_181 =
  doubleSided $ (story ":circus-ex-mortis:181" "Path Forward" Set.RedSunrise) {cdEncounterSetQuantity = Just 2}

pathForward_182 :: CardDef
pathForward_182 =
  doubleSided $ (story ":circus-ex-mortis:182" "Path Forward" Set.RedSunrise) {cdEncounterSetQuantity = Just 2}

pathForward_183 :: CardDef
pathForward_183 =
  doubleSided $ (story ":circus-ex-mortis:183" "Path Forward" Set.RedSunrise) {cdEncounterSetQuantity = Just 2}

-- Circus Ex Mortis (fan campaign by Tyler Gotch): thousand_to_one
strikeTheHeart :: CardDef
strikeTheHeart =
  doubleSided $ story ":circus-ex-mortis:201" "Strike the Heart" Set.ThousandToOne

silenceThePipes :: CardDef
silenceThePipes =
  doubleSided $ story ":circus-ex-mortis:202" "Silence the Pipes" Set.ThousandToOne

raiseTheTorch :: CardDef
raiseTheTorch =
  doubleSided $ story ":circus-ex-mortis:203" "Raise the Torch" Set.ThousandToOne

splitTheRock :: CardDef
splitTheRock =
  doubleSided $ story ":circus-ex-mortis:204" "Split the Rock" Set.ThousandToOne

scribeTheSigil :: CardDef
scribeTheSigil =
  doubleSided $ story ":circus-ex-mortis:205" "Scribe the Sigil" Set.ThousandToOne

cleanseTheStain :: CardDef
cleanseTheStain =
  doubleSided $ story ":circus-ex-mortis:206" "Cleanse the Stain" Set.ThousandToOne

reciteThePrayer :: CardDef
reciteThePrayer =
  doubleSided $ story ":circus-ex-mortis:207" "Recite the Prayer" Set.ThousandToOne

bearTheBurden :: CardDef
bearTheBurden =
  doubleSided $ story ":circus-ex-mortis:208" "Bear the Burden" Set.ThousandToOne
