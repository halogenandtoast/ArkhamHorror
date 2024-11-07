module Arkham.Asset.Cards.ThePathToCarcosa where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

sophieInLovingMemory :: CardDef
sophieInLovingMemory =
  signature "03001"
    $ (asset "03009" ("Sophie" <:> "In Loving Memory") 0 Neutral)
      { cdCardTraits = setFromList [Item, Spirit]
      , cdUnique = True
      , cdCost = Nothing
      }

sophieItWasAllMyFault :: CardDef
sophieItWasAllMyFault =
  signature "03001"
    $ (asset "03009b" ("Sophie" <:> "It Was All My Fault") 0 Neutral)
      { cdCardTraits = setFromList [Item, Madness]
      , cdUnique = True
      , cdCost = Nothing
      }

analyticalMind :: CardDef
analyticalMind =
  signature "03002"
    $ (asset "03010" ("Analytical Mind" <:> "Between the Lines") 3 Neutral)
      { cdCardTraits = singleton Talent
      , cdSkills = [#wild, #wild]
      }

theKingInYellow :: CardDef
theKingInYellow =
  (weakness "03011" ("The King in Yellow" <:> "Act 1"))
    { cdCardTraits = singleton Tome
    , cdUnique = True
    , cdSlots = [#hand]
    }

spiritSpeaker :: CardDef
spiritSpeaker =
  signature "03004"
    $ (asset "03014" ("Spirit-Speaker" <:> "Envoy of the Alusi") 2 Neutral)
      { cdSkills = [#willpower, #intellect, #wild]
      , cdCardTraits = singleton Ritual
      }

thirtyTwoColt :: CardDef
thirtyTwoColt =
  (asset "03020" ".32 Colt" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUses = uses Ammo 6
    , cdSlots = [#hand]
    }

trueGrit :: CardDef
trueGrit =
  (asset "03021" "True Grit" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    }

fieldwork :: CardDef
fieldwork =
  (asset "03024" "Fieldwork" 2 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Talent
    }

archaicGlyphs :: CardDef
archaicGlyphs =
  (asset "03025" ("Archaic Glyphs" <:> "Untranslated") 0 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Occult, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 0
    }

inTheKnow1 :: CardDef
inTheKnow1 =
  (asset "03027" "In the Know" 3 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Talent
    , cdUses = uses Secret 3
    , cdLevel = Just 1
    }

stealth :: CardDef
stealth =
  (asset "03028" "Stealth" 2 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Talent
    }

lockpicks1 :: CardDef
lockpicks1 =
  (asset "03031" "Lockpicks" 3 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool, Illicit]
    , cdUses = uses Supply 3
    , cdLevel = Just 1
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01687"]
    }

alchemicalTransmutation :: CardDef
alchemicalTransmutation =
  (asset "03032" "Alchemical Transmutation" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

spiritAthame1 :: CardDef
spiritAthame1 =
  (asset "03035" "Spirit Athame" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdLevel = Just 1
    , cdSlots = [#hand]
    }

lantern :: CardDef
lantern =
  (asset "03036" "Lantern" 2 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    }

gravediggersShovel :: CardDef
gravediggersShovel =
  (asset "03037" "Gravedigger's Shovel" 2 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdSlots = [#hand]
    }

constanceDumaine :: CardDef
constanceDumaine =
  (storyAsset "03076a" ("Constance Dumaine" <:> "Sociable Hostess") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

jordanPerry :: CardDef
jordanPerry =
  (storyAsset "03077" ("Jordan Perry" <:> "Dignified Financier") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

ishimaruHaruko :: CardDef
ishimaruHaruko =
  (storyAsset "03078" ("Ishimaru Haruko" <:> "Costume Designer") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

sebastienMoreau :: CardDef
sebastienMoreau =
  (storyAsset "03079" ("Sebastien Moreau" <:> "Impassioned Producer") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

ashleighClarke :: CardDef
ashleighClarke =
  (storyAsset "03080" ("Ashleigh Clarke" <:> "Talented Entertainer") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

combatTraining1 :: CardDef
combatTraining1 =
  (asset "03107" "Combat Training" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Talent, Composure]
    , cdLimits = [LimitPerTrait Composure 1]
    , cdLevel = Just 1
    }

scientificTheory1 :: CardDef
scientificTheory1 =
  fast
    $ (asset "03109" "Scientific Theory" 1 Seeker)
      { cdSkills = [#intellect, #combat]
      , cdCardTraits = setFromList [Talent, Composure]
      , cdLimits = [LimitPerTrait Composure 1]
      , cdLevel = Just 1
      }

knuckleduster :: CardDef
knuckleduster =
  (asset "03110" "Knuckleduster" 2 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
    , cdSlots = [#hand]
    }

moxie1 :: CardDef
moxie1 =
  (asset "03111" "Moxie" 1 Rogue)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Talent, Composure]
    , cdLimits = [LimitPerTrait Composure 1]
    , cdLevel = Just 1
    }

davidRenfield :: CardDef
davidRenfield =
  (asset "03112" ("David Renfield" <:> "Esteemed Eschatologist") 2 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }

grounded1 :: CardDef
grounded1 =
  (asset "03113" "Grounded" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Talent, Composure]
    , cdLimits = [LimitPerTrait Composure 1]
    , cdLevel = Just 1
    }

cherishedKeepsake :: CardDef
cherishedKeepsake =
  (asset "03114" "Cherished Keepsake" 0 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    }

plucky1 :: CardDef
plucky1 =
  (asset "03115" "Plucky" 1 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Talent, Composure]
    , cdLimits = [LimitPerTrait Composure 1]
    , cdLevel = Just 1
    }

mrPeabody :: CardDef
mrPeabody =
  (storyAsset "03141" ("Mr. Peabody" <:> "Historical Society Curator") 0 EchoesOfThePast)
    { cdCardTraits = setFromList [Ally, HistoricalSociety]
    , cdCost = Nothing
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdSlots = [#ally]
    }

claspOfBlackOnyx :: CardDef
claspOfBlackOnyx =
  (storyWeakness "03142" ("Clasp of Black Onyx" <:> "A Gift Unlooked For") EchoesOfThePast)
    { cdCardTraits = setFromList [Item, Relic]
    , cdCost = Just (StaticCost 1)
    , cdRevelation = NoRevelation
    , cdCardInHandEffects = True
    }

theTatteredCloak :: CardDef
theTatteredCloak =
  (storyAsset "03143" ("The Tattered Cloak" <:> "Regalia Dementia") 2 EchoesOfThePast)
    { cdSkills = [#willpower, #combat, #agility]
    , cdCardTraits = setFromList [Item, Clothing]
    , cdSlots = [#body]
    }

trenchKnife :: CardDef
trenchKnife =
  (asset "03147" "Trench Knife" 1 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

charlesRossEsq :: CardDef
charlesRossEsq =
  (asset "03149" ("Charles Ross, Esq." <:> "Acquisitions and Solicitation") 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }

darioElAmin :: CardDef
darioElAmin =
  (asset "03151" ("Dario El-Amin" <:> "Unscrupulous Investor") 4 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }

bookOfShadows1 :: CardDef
bookOfShadows1 =
  (asset "03154" "Book of Shadows" 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdLevel = Just 1
    , cdSlots = [#hand]
    }

danielChesterfield :: CardDef
danielChesterfield =
  ( storyAsset "03182a" ("Daniel Chesterfield" <:> "He's Not Doing All Too Well") 0 TheUnspeakableOath
  )
    { cdCardTraits = setFromList [Ally, Lunatic]
    , cdCost = Nothing
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdOtherSide = Just "03182b"
    , cdSlots = [#ally]
    }

straitjacket :: CardDef
straitjacket =
  (storyAsset "x03185" "Straitjacket" 0 TheUnspeakableOath)
    { cdCardTraits = setFromList [Item, Clothing]
    , cdCardType = EncounterAssetType
    , cdSlots = [#body, #hand, #hand]
    , cdEncounterSetQuantity = Just 2
    , cdCost = Nothing
    , cdClassSymbols = singleton Mythos
    }

fortyFiveAutomatic2 :: CardDef
fortyFiveAutomatic2 =
  (asset "03190" ".45 Automatic" 4 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 4
    , cdLevel = Just 2
    }

archaicGlyphsGuidingStones3 :: CardDef
archaicGlyphsGuidingStones3 =
  (asset "03192" ("Archaic Glyphs" <:> "Guiding Stones") 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = Just 3
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheGlyphs
    }

archaicGlyphsProphecyForetold3 :: CardDef
archaicGlyphsProphecyForetold3 =
  (asset "03193" ("Archaic Glyphs" <:> "Prophecy Foretold") 2 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = Just 3
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheGlyphs
    }

pickpocketing2 :: CardDef
pickpocketing2 =
  fast
    $ (asset "03195" "Pickpocketing" 2 Rogue)
      { cdSkills = [#agility, #agility]
      , cdCardTraits = setFromList [Talent, Illicit]
      , cdLevel = Just 2
      }

madameLabranche :: CardDef
madameLabranche =
  (asset "03198" ("Madame Labranche" <:> "Mysterious Benefactress") 2 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }

firstAid3 :: CardDef
firstAid3 =
  (asset "03230" "First Aid" 2 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Talent, Science]
    , cdUses = uses Supply 4
    , cdLevel = Just 3
    , cdAlternateCardCodes = ["01683"]
    }

fortyOneDerringer2 :: CardDef
fortyOneDerringer2 =
  (asset "03234" ".41 Derringer" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand]
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01688"]
    }

scrying3 :: CardDef
scrying3 =
  (asset "03236" "Scrying" 1 Mystic)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = Just 3
    , cdAlternateCardCodes = ["01690"]
    }

stickToThePlan3 :: CardDef
stickToThePlan3 =
  permanent
    $ (asset "03264" "Stick to the Plan" 0 Guardian)
      { cdCardTraits = singleton Talent
      , cdKeywords = setFromList [Keyword.Permanent, Keyword.Exceptional]
      , cdLevel = Just 3
      }

arcaneInsight4 :: CardDef
arcaneInsight4 =
  (asset "03266" "Arcane Insight" 3 Seeker)
    { cdCardTraits = singleton Spell
    , cdSkills = [#willpower, #intellect]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = Just 4
    }

suggestion4 :: CardDef
suggestion4 =
  (asset "03268" "Suggestion" 3 Rogue)
    { cdCardTraits = singleton Spell
    , cdSkills = [#willpower, #agility]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = Just 4
    }

stHubertsKey :: CardDef
stHubertsKey =
  (asset "03269" ("St. Hubert's Key" <:> "Cleansing Fire") 4 Mystic)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSkills = [#willpower]
    , cdSlots = [#accessory]
    , cdUnique = True
    }

arcaneInitiate3 :: CardDef
arcaneInitiate3 =
  (asset "03271" "Arcane Initiate" 0 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Ally, Sorcerer]
    , cdSlots = [#ally]
    , cdLevel = Just 3
    }

armorOfArdennes5 :: CardDef
armorOfArdennes5 =
  (asset "03305" "Armor of Ardennes" 4 Guardian)
    { cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdCardTraits = setFromList [Item, Armor, Relic]
    , cdSlots = [#body]
    , cdLevel = Just 5
    }

charonsObol1 :: CardDef
charonsObol1 =
  permanent
    $ (asset "03308" ("Charon's Obol" <:> "The Ferryman's Pay") 0 Rogue)
      { cdCardTraits = setFromList [Item, Relic]
      , cdLevel = Just 1
      , cdKeywords = setFromList [Keyword.Permanent, Keyword.Exceptional]
      , cdUnique = True
      }

lupara3 :: CardDef
lupara3 =
  (asset "03309" "Lupara" 3 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdLevel = Just 3
    , cdUses = uses Ammo 2
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdSlots = [#hand]
    }

newspaper2 :: CardDef
newspaper2 =
  (asset "03313" "Newspaper" 1 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Item
    , cdLevel = Just 2
    , cdSlots = [#hand]
    }

keyOfYs :: CardDef
keyOfYs =
  (asset "03315" ("Key of Ys" <:> "Let the Storm Rage") 3 Neutral)
    { cdSkills = [#wild, #willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = Just 5
    , cdSlots = [#accessory]
    , cdUnique = True
    }

thePallidMask :: CardDef
thePallidMask =
  (asset "03321b" ("The Pallid Mask" <:> "Chasing Tails") 0 Neutral)
    { cdCardTraits = setFromList [Item, Relic]
    , cdRevelation = IsRevelation
    , cdUnique = True
    , cdLevel = Nothing
    }

courage :: CardDef
courage =
  (asset "xcourage" "Courage" 0 Neutral) {cdCardTraits = singleton Courage}
