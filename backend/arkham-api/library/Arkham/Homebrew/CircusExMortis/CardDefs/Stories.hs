module Arkham.Homebrew.CircusExMortis.CardDefs.Stories where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.Traits
import Arkham.Prelude
import Arkham.Story.CardDefs.Base
import Arkham.Trait (Trait (Bystander))

-- Circus Ex Mortis (fan campaign by Tyler Gotch): harm_s_way
theDarkYoungStir :: CardDef
theDarkYoungStir =
  doubleSided $ story ":circus-ex-mortis:058" "The Dark Young Stir..." Set.HarmsWay

-- The Kidnapped Citizen face is the Bystander side; the trait belongs to the
-- card, so the story entity carries it on either face.
hiddenInPlainSight :: CardDef
hiddenInPlainSight =
  addTrait Bystander
    $ doubleSided
    $ story ":circus-ex-mortis:059" "Hidden in Plain Sight" Set.HarmsWay

underLockAndKey :: CardDef
underLockAndKey =
  addTrait Bystander $ doubleSided $ story ":circus-ex-mortis:060" "Under Lock and Key" Set.HarmsWay

cautiousJailers :: CardDef
cautiousJailers =
  addTrait Bystander $ doubleSided $ story ":circus-ex-mortis:061" "Cautious Jailers" Set.HarmsWay

deepInTheDark :: CardDef
deepInTheDark =
  addTrait Bystander $ doubleSided $ story ":circus-ex-mortis:062" "Deep in the Dark" Set.HarmsWay

clappedInIrons :: CardDef
clappedInIrons =
  addTrait Bystander $ doubleSided $ story ":circus-ex-mortis:063" "Clapped in Irons" Set.HarmsWay

hypnoticState :: CardDef
hypnoticState =
  addTrait Bystander $ doubleSided $ story ":circus-ex-mortis:064" "Hypnotic State" Set.HarmsWay

-- Circus Ex Mortis (fan campaign by Tyler Gotch): red_sunrise

{- | Path Forward is printed once per column with 2 copies each, and setup can put two
copies of the SAME column on the board (one beside each of two rows). 'Arkham.Id.StoryId'
is the card code, so both copies would collide in the story map and one would silently
vanish -- taking a row's only way up with it. Each physical copy therefore gets its own
code -- suffixed @a@, never @b@, because 'flippedCardCode' already means @b@ is a
card's BACK: a copy at @...178b@ would be the printed card's back face, and its own
back would resolve to the printed card's front. The copies carry 'cdDuplicateOf' so the
card browser still lists four cards,
and 'cdArt' so they draw the printed card's art.
-}
pathForwardCopy :: CardDef -> CardDef
pathForwardCopy printed =
  ( doubleSided
      $ (story (CardCode $ unCardCode (toCardCode printed) <> "a") "Path Forward" Set.RedSunrise)
        { cdEncounterSetQuantity = Just 1
        }
  )
    { -- Both faces are the printed card's; only the code differs.
      cdArt = cdArt printed
    , cdOtherSide = Just $ flippedCardCode (toCardCode printed)
    , cdDuplicateOf = Just (toCardCode printed)
    }

pathForward :: CardCode -> CardDef
pathForward code =
  doubleSided $ (story code "Path Forward" Set.RedSunrise) {cdEncounterSetQuantity = Just 1}

pathForward_178 :: CardDef
pathForward_178 = pathForward ":circus-ex-mortis:178"

pathForward_178a :: CardDef
pathForward_178a = pathForwardCopy pathForward_178

pathForward_179 :: CardDef
pathForward_179 = pathForward ":circus-ex-mortis:179"

pathForward_179a :: CardDef
pathForward_179a = pathForwardCopy pathForward_179

pathForward_180 :: CardDef
pathForward_180 = pathForward ":circus-ex-mortis:180"

pathForward_180a :: CardDef
pathForward_180a = pathForwardCopy pathForward_180

pathForward_181 :: CardDef
pathForward_181 = pathForward ":circus-ex-mortis:181"

pathForward_181a :: CardDef
pathForward_181a = pathForwardCopy pathForward_181

-- Circus Ex Mortis (fan campaign by Tyler Gotch): thousand_to_one
strikeTheHeart :: CardDef
strikeTheHeart =
  addTrait Destiny $ doubleSided $ story ":circus-ex-mortis:201" "Strike the Heart" Set.ThousandToOne

silenceThePipes :: CardDef
silenceThePipes =
  addTrait Destiny $ doubleSided $ story ":circus-ex-mortis:202" "Silence the Pipes" Set.ThousandToOne

raiseTheTorch :: CardDef
raiseTheTorch =
  addTrait Destiny $ doubleSided $ story ":circus-ex-mortis:203" "Raise the Torch" Set.ThousandToOne

splitTheRock :: CardDef
splitTheRock =
  addTrait Destiny $ doubleSided $ story ":circus-ex-mortis:204" "Split the Rock" Set.ThousandToOne

scribeTheSigil :: CardDef
scribeTheSigil =
  addTrait Destiny $ doubleSided $ story ":circus-ex-mortis:205" "Scribe the Sigil" Set.ThousandToOne

cleanseTheStain :: CardDef
cleanseTheStain =
  addTrait Destiny $ doubleSided $ story ":circus-ex-mortis:206" "Cleanse the Stain" Set.ThousandToOne

reciteThePrayer :: CardDef
reciteThePrayer =
  addTrait Destiny $ doubleSided $ story ":circus-ex-mortis:207" "Recite the Prayer" Set.ThousandToOne

bearTheBurden :: CardDef
bearTheBurden =
  addTrait Destiny $ doubleSided $ story ":circus-ex-mortis:208" "Bear the Burden" Set.ThousandToOne
