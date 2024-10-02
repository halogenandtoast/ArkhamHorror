module Arkham.Asset.Cards.TheDunwichLegacy where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

zoeysCross :: CardDef
zoeysCross =
  signature "02001"
    $ (asset "02006" ("Zoey's Cross" <:> "Symbol of Righteousness") 1 Neutral)
      { cdSkills = [#combat, #combat, #wild]
      , cdCardTraits = setFromList [Item, Charm]
      , cdUnique = True
      , cdSlots = [#accessory]
      }

jennysTwin45s :: CardDef
jennysTwin45s =
  signature "02003"
    $ (asset "02010" ("Jenny's Twin .45s" <:> "A Perfect Fit") 0 Neutral)
      { cdSkills = [#agility, #agility, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdCost = Just DynamicCost
      , cdUnique = True
      , cdSlots = [#hand, #hand]
      }

jimsTrumpet :: CardDef
jimsTrumpet =
  signature "02004"
    $ (asset "02012" ("Jim's Trumpet" <:> "The Dead Listen") 2 Neutral)
      { cdSkills = [#willpower, #willpower, #wild]
      , cdCardTraits = setFromList [Item, Instrument, Relic]
      , cdUnique = True
      , cdSlots = [#hand]
      }

duke :: CardDef
duke =
  signature "02005"
    $ (asset "02014" ("Duke" <:> "Loyal Hound") 2 Neutral)
      { cdCardTraits = setFromList [Ally, Creature]
      , cdUnique = True
      }

blackjack :: CardDef
blackjack =
  (asset "02016" "Blackjack" 1 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat]
    , cdSlots = [#hand]
    }

laboratoryAssistant :: CardDef
laboratoryAssistant =
  (asset "02020" "Laboratory Assistant" 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic, Science]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["60212"]
    }

strangeSolution :: CardDef
strangeSolution =
  (asset "02021" ("Strange Solution" <:> "Unidentified") 1 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Science]
    }

liquidCourage :: CardDef
liquidCourage =
  (asset "02024" "Liquid Courage" 1 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 4
    }

hiredMuscle1 :: CardDef
hiredMuscle1 =
  (asset "02027" "Hired Muscle" 1 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdLevel = Just 1
    , cdSlots = [#ally]
    }

riteOfSeeking :: CardDef
riteOfSeeking =
  (asset "02028" "Rite of Seeking" 4 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

ritualCandles :: CardDef
ritualCandles =
  (asset "02029" "Ritual Candles" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Item
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["60405"]
    }

clarityOfMind :: CardDef
clarityOfMind =
  (asset "02030" "Clarity of Mind" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

fireAxe :: CardDef
fireAxe =
  (asset "02032" "Fire Axe" 1 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

peterSylvestre :: CardDef
peterSylvestre =
  (asset "02033" ("Peter Sylvestre" <:> "Big Man on Campus") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

peterSylvestre2 :: CardDef
peterSylvestre2 =
  (asset "02035" ("Peter Sylvestre" <:> "Big Man on Campus") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdLevel = Just 2
    , cdUnique = True
    , cdSlots = [#ally]
    }

kukri :: CardDef
kukri =
  (asset "02036" "Kukri" 2 Neutral)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

drHenryArmitage :: CardDef
drHenryArmitage =
  (storyAsset "02040" ("Dr. Henry Armitage" <:> "The Head Librarian") 2 ArmitagesFate)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

alchemicalConcoction :: CardDef
alchemicalConcoction =
  (storyAsset "02059" "Alchemical Concoction" 0 ExtracurricularActivity)
    { cdCardTraits = setFromList [Item, Science]
    , cdCardType = EncounterAssetType
    }

jazzMulligan :: CardDef
jazzMulligan =
  (storyAsset "02060" ("\"Jazz\" Mulligan" <:> "The Head Janitor") 0 ExtracurricularActivity)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

professorWarrenRice :: CardDef
professorWarrenRice =
  ( storyAsset "02061" ("Professor Warren Rice" <:> "Professor of Languages") 3 ExtracurricularActivity
  )
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

peterClover :: CardDef
peterClover =
  (storyAsset "02079" ("Peter Clover" <:> "Holding All the Cards") 0 TheHouseAlwaysWins)
    { cdCardTraits = setFromList [Humanoid, Criminal]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

drFrancisMorgan :: CardDef
drFrancisMorgan =
  (storyAsset "02080" ("Dr. Francis Morgan" <:> "Professor of Archaeology") 3 TheHouseAlwaysWins)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

brotherXavier1 :: CardDef
brotherXavier1 =
  (asset "02106" ("Brother Xavier" <:> "Pure of Spirit") 5 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally]
    , cdLevel = Just 1
    , cdUnique = True
    , cdSlots = [#ally]
    }

pathfinder1 :: CardDef
pathfinder1 =
  (asset "02108" "Pathfinder" 3 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Talent
    , cdLevel = Just 1
    }

adaptable1 :: CardDef
adaptable1 =
  permanent
    $ (asset "02110" "Adaptable" 0 Rogue)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = Just 1
      }

songOfTheDead2 :: CardDef
songOfTheDead2 =
  (asset "02112" "Song of the Dead" 2 Mystic)
    { cdCardTraits = setFromList [Spell, Song]
    , cdSkills = [#willpower]
    , cdLevel = Just 2
    , cdUses = uses Charge 5
    , cdSlots = [#arcane]
    }

fireExtinguisher1 :: CardDef
fireExtinguisher1 =
  (asset "02114" "Fire Extinguisher" 2 Survivor)
    { cdCardTraits = setFromList [Item, Tool, Melee]
    , cdSkills = [#agility]
    , cdLevel = Just 1
    , cdSlots = [#hand]
    }

smokingPipe :: CardDef
smokingPipe =
  (asset "02116" "Smoking Pipe" 1 Neutral)
    { cdCardTraits = singleton Item
    , cdSkills = [#willpower]
    , cdUses = uses Supply 3
    }

painkillers :: CardDef
painkillers =
  (asset "02117" "Painkillers" 1 Neutral)
    { cdCardTraits = singleton Item
    , cdSkills = [#willpower]
    , cdUses = uses Supply 3
    }

haroldWalsted :: CardDef
haroldWalsted =
  (storyAsset "02138" ("Harold Walsted" <:> "Curator of the Museum") 0 TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

adamLynch :: CardDef
adamLynch =
  (storyAsset "02139" ("Adam Lynch" <:> "Museum Security") 0 TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

theNecronomiconOlausWormiusTranslation :: CardDef
theNecronomiconOlausWormiusTranslation =
  (storyAsset "02140" ("The Necronomicon" <:> "Olaus Wormius Translation") 2 TheMiskatonicMuseum)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    }

bandolier :: CardDef
bandolier =
  (asset "02147" "Bandolier" 2 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item]
    , cdSlots = [#body]
    }

artStudent :: CardDef
artStudent =
  (asset "02149" "Art Student" 2 Seeker)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSkills = [#intellect]
    , cdSlots = [#ally]
    }

switchblade2 :: CardDef
switchblade2 =
  fast
    $ (asset "02152" "Switchblade" 1 Rogue)
      { cdSkills = [#combat, #agility]
      , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
      , cdLevel = Just 2
      , cdSlots = [#hand]
      }

shrivelling3 :: CardDef
shrivelling3 =
  (asset "02154" "Shrivelling" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Spell
    , cdLevel = Just 3
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    }

newspaper :: CardDef
newspaper =
  (asset "02155" "Newspaper" 1 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Item
    , cdSlots = [#hand]
    }

relicHunter3 :: CardDef
relicHunter3 =
  permanent
    $ (asset "02157" "Relic Hunter" 0 Neutral)
      { cdCardTraits = singleton Talent
      , cdLevel = Just 3
      , cdAlternateCardCodes = ["01695"]
      }

charisma3 :: CardDef
charisma3 =
  permanent
    $ (asset "02158" "Charisma" 0 Neutral)
      { cdCardTraits = singleton Talent
      , cdLevel = Just 3
      , cdAlternateCardCodes = ["01694"]
      }

helplessPassenger :: CardDef
helplessPassenger =
  (storyAsset "02179" "Helpless Passenger" 0 TheEssexCountyExpress)
    { cdCardTraits = setFromList [Ally, Bystander]
    , cdKeywords = singleton Keyword.Surge
    , cdEncounterSetQuantity = Just 3
    , cdCardType = EncounterAssetType
    }

keenEye3 :: CardDef
keenEye3 =
  permanent
    $ (asset "02185" "Keen Eye" 0 Guardian)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = Just 3
      }

higherEducation3 :: CardDef
higherEducation3 =
  permanent
    $ (asset "02187" "Higher Education" 0 Seeker)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = Just 3
      }

loneWolf :: CardDef
loneWolf =
  (asset "02188" "Lone Wolf" 1 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Talent]
    , cdLimits = [LimitPerInvestigator 1]
    }

streetwise3 :: CardDef
streetwise3 =
  permanent
    $ (asset "02189" "Streetwise" 0 Rogue)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = Just 3
      }

bloodPact3 :: CardDef
bloodPact3 =
  permanent
    $ (asset "02191" "Blood Pact" 0 Mystic)
      { cdCardTraits = setFromList [Spell, Pact]
      , cdLevel = Just 3
      }

scrapper3 :: CardDef
scrapper3 =
  permanent
    $ (asset "02193" "Scrapper" 0 Survivor)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = Just 3
      }

keyToTheChamber :: CardDef
keyToTheChamber =
  (storyAsset "02215" "Key to the Chamber" 0 BloodOnTheAltar)
    { cdCardTraits = setFromList [Item, Key]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

zebulonWhateley :: CardDef
zebulonWhateley =
  (storyAsset "02217" ("Zebulon Whateley" <:> "Recalling Ancient Things") 3 BloodOnTheAltar)
    { cdCardTraits = setFromList [Ally, Dunwich]
    , cdSkills = [#willpower, #wild]
    , cdUnique = True
    , cdSlots = [#ally]
    }

earlSawyer :: CardDef
earlSawyer =
  (storyAsset "02218" ("Earl Sawyer" <:> "Smarter Than He Lets On") 3 BloodOnTheAltar)
    { cdCardTraits = setFromList [Ally, Dunwich]
    , cdSkills = [#agility, #wild]
    , cdUnique = True
    , cdSlots = [#ally]
    }

powderOfIbnGhazi :: CardDef
powderOfIbnGhazi =
  (storyAsset "02219" ("Powder of Ibn-Ghazi" <:> "Seeing Things Unseen") 0 BloodOnTheAltar)
    { cdCardTraits = singleton Item
    }

springfieldM19034 :: CardDef
springfieldM19034 =
  (asset "02226" "Springfield M1903" 4 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdLevel = Just 4
    , cdSkills = [#combat, #agility]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand, #hand]
    }

luckyDice2 :: CardDef
luckyDice2 =
  (asset "02230" ("Lucky Dice" <:> "...Or Are They?") 2 Rogue)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#willpower, #agility]
    , cdExceptional = True
    , cdLevel = Just 2
    , cdSlots = [#accessory]
    }

alyssaGraham :: CardDef
alyssaGraham =
  (asset "02232" ("Alyssa Graham" <:> "Speaker to the Dead") 4 Mystic)
    { cdCardTraits = setFromList [Ally, Sorcerer]
    , cdSkills = [#intellect]
    , cdUnique = True
    , cdSlots = [#ally]
    }

riteOfSeeking4 :: CardDef
riteOfSeeking4 =
  (asset "02233" "Rite of Seeking" 5 Mystic)
    { cdCardTraits = singleton Spell
    , cdSkills = [#intellect, #intellect]
    , cdLevel = Just 4
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

darkHorse :: CardDef
darkHorse =
  (asset "02234" "Dark Horse" 3 Survivor)
    { cdCardTraits = singleton Condition
    , cdSkills = [#willpower]
    , cdLimits = [LimitPerInvestigator 1]
    }

esotericFormula :: CardDef
esotericFormula =
  (storyAsset "02254" "Esoteric Formula" 0 UndimensionedAndUnseen)
    { cdCardTraits = singleton Spell
    , cdEncounterSetQuantity = Just 4
    }

strangeSolutionRestorativeConcoction4 :: CardDef
strangeSolutionRestorativeConcoction4 =
  (asset "02262" ("Strange Solution" <:> "Restorative Concoction") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#willpower, #willpower]
    , cdLevel = Just 4
    , cdUses = uses Supply 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheSolution
    }

strangeSolutionAcidicIchor4 :: CardDef
strangeSolutionAcidicIchor4 =
  (asset "02263" ("Strange Solution" <:> "Acidic Ichor") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#combat, #combat]
    , cdLevel = Just 4
    , cdUses = uses Supply 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheSolution
    }

strangeSolutionFreezingVariant4 :: CardDef
strangeSolutionFreezingVariant4 =
  (asset "02264" ("Strange Solution" <:> "Freezing Variant") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#agility, #agility]
    , cdLevel = Just 4
    , cdUses = uses Supply 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheSolution
    }

joeyTheRatVigil :: CardDef
joeyTheRatVigil =
  (asset "02265" ("Joey \"The Rat\" Vigil" <:> "Lookin' Out for #1") 4 Rogue)
    { cdCardTraits = setFromList [Ally, Criminal]
    , cdSkills = [#intellect, #agility]
    , cdUnique = True
    , cdSlots = [#ally]
    }

jewelOfAureolus3 :: CardDef
jewelOfAureolus3 =
  (asset "02269" ("Jewel of Aureolus" <:> "Gift of the Homunculi") 3 Mystic)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#wild]
    , cdLevel = Just 3
    , cdUnique = True
    , cdSlots = [#accessory]
    }

fineClothes :: CardDef
fineClothes =
  (asset "02272" "Fine Clothes" 1 Neutral)
    { cdCardTraits = setFromList [Item, Clothing]
    , cdSkills = [#agility]
    , cdSlots = [#body]
    }

lightningGun5 :: CardDef
lightningGun5 =
  (asset "02301" "Lightning Gun" 6 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdLevel = Just 5
    , cdSkills = [#intellect, #combat]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand, #hand]
    }

drWilliamTMaleson :: CardDef
drWilliamTMaleson =
  (asset "02302" ("Dr. William T. Maleson" <:> "Working on Something Big") 1 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

chicagoTypewriter4 :: CardDef
chicagoTypewriter4 =
  (asset "02304" "Chicago Typewriter" 5 Rogue)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdLevel = Just 4
    , cdUses = uses Ammo 4
    , cdSlots = [#hand, #hand]
    }

theGoldPocketWatch4 :: CardDef
theGoldPocketWatch4 =
  (asset "02305" ("The Gold Pocket Watch" <:> "Stealing Time") 2 Rogue)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = Just 4
    , cdUnique = True
    , cdExceptional = True
    , cdSlots = [#accessory]
    }

shrivelling5 :: CardDef
shrivelling5 =
  (asset "02306" "Shrivelling" 3 Mystic)
    { cdSkills = [#willpower, #combat, #combat]
    , cdCardTraits = singleton Spell
    , cdLevel = Just 5
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    }

aquinnah3 :: CardDef
aquinnah3 =
  (asset "02308" ("Aquinnah" <:> "The Forgotten Daughter") 4 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Ally]
    , cdLevel = Just 3
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01691"]
    }

tryAndTryAgain3 :: CardDef
tryAndTryAgain3 =
  (asset "02309" "Try and Try Again" 2 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Talent
    , cdLevel = Just 3
    }

theRedGlovedMan5 :: CardDef
theRedGlovedMan5 =
  fast
    $ (asset "02310" ("The Red-Gloved Man" <:> "He Was Never There") 2 Neutral)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Ally, Conspirator]
      , cdLevel = Just 5
      , cdUnique = True
      , cdSlots = [#ally]
      }
