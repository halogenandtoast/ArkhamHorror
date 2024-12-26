module Arkham.Asset.Cards.EdgeOfTheEarth where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait

mechanicsWrench :: CardDef
mechanicsWrench =
  signature "08001"
    $ (asset "08002" "Mechanic's Wrench" 2 Neutral)
      { cdCardTraits = setFromList [Item, Tool, Melee]
      , cdSkills = [#combat, #combat, #wild]
      , cdSlots = [#hand]
      }

livreDeibon :: CardDef
livreDeibon =
  signature "08004"
    $ (asset "08005" ("Livre d'Eibon" <:> "Hyperborean Grimoire") 2 Neutral)
      { cdCardTraits = setFromList [Item, Relic, Tome]
      , cdSkills = [#willpower, #willpower, #wild]
      , cdUnique = True
      , cdSlots = [#hand]
      }

trustyBullwhip :: CardDef
trustyBullwhip =
  signature "08007"
    . fast
    $ (asset "08008" "Trusty Bullwhip" 2 Neutral)
      { cdCardTraits = setFromList [Item, Weapon, Melee]
      , cdSkills = [#agility, #agility, #wild]
      , cdSlots = [#hand]
      }

disciplineAlignmentOfSpirit :: CardDef
disciplineAlignmentOfSpirit =
  signature "08010"
    . permanent
    $ (asset "08011a" ("Discipline" <:> "Alignment of Spirit") 0 Neutral)
      { cdCardTraits = setFromList [Unbroken]
      , cdOtherSide = Just "08011b"
      }

disciplineAlignmentOfSpiritBroken :: CardDef
disciplineAlignmentOfSpiritBroken =
  signature "08010"
    . permanent
    $ (asset "08011b" ("Discipline" <:> "Alignment of Spirit") 0 Neutral)
      { cdCardTraits = setFromList [Broken]
      , cdOtherSide = Just "08011a"
      }

disciplineQuiescenceOfThought :: CardDef
disciplineQuiescenceOfThought =
  signature "08010"
    . permanent
    $ (asset "08012a" ("Discipline" <:> "Quiescence of Thought") 0 Neutral)
      { cdCardTraits = setFromList [Unbroken]
      , cdOtherSide = Just "08012b"
      }

disciplineQuiescenceOfThoughtBroken :: CardDef
disciplineQuiescenceOfThoughtBroken =
  signature "08010"
    . permanent
    $ (asset "08012b" ("Discipline" <:> "Quiescence of Thought") 0 Neutral)
      { cdCardTraits = setFromList [Broken]
      , cdOtherSide = Just "08012a"
      }

disciplinePrescienceOfFate :: CardDef
disciplinePrescienceOfFate =
  signature "08010"
    . permanent
    $ (asset "08013a" ("Discipline" <:> "Prescience of Fate") 0 Neutral)
      { cdCardTraits = setFromList [Unbroken]
      , cdOtherSide = Just "08013b"
      }

disciplinePrescienceOfFateBroken :: CardDef
disciplinePrescienceOfFateBroken =
  signature "08010"
    . permanent
    $ (asset "08013b" ("Discipline" <:> "Prescience of Fate") 0 Neutral)
      { cdCardTraits = setFromList [Broken]
      , cdOtherSide = Just "08013a"
      }

disciplineBalanceOfBody :: CardDef
disciplineBalanceOfBody =
  signature "08010"
    . permanent
    $ (asset "08014a" ("Discipline" <:> "Balance of Body") 0 Neutral)
      { cdCardTraits = setFromList [Unbroken]
      , cdOtherSide = Just "08014b"
      }

disciplineBalanceOfBodyBroken :: CardDef
disciplineBalanceOfBodyBroken =
  signature "08010"
    . permanent
    $ (asset "08014b" ("Discipline" <:> "Balance of Body") 0 Neutral)
      { cdCardTraits = setFromList [Broken]
      , cdOtherSide = Just "08014a"
      }

shrewdDealings :: CardDef
shrewdDealings =
  signature "08016"
    $ (asset "08017" "Shrewd Dealings" 2 Neutral)
      { cdSkills = [#intellect, #intellect, #wild]
      , cdCardTraits = setFromList [Talent]
      }

gearedUp :: CardDef
gearedUp =
  permanent
    $ (asset "08019" "Geared Up" 0 Guardian)
      { cdCardTraits = setFromList [Talent]
      , cdDeckRestrictions = [PerDeckLimit 1, PurchaseAtDeckCreation]
      }

butterflySwords2 :: CardDef
butterflySwords2 =
  (asset "08025" "Butterfly Swords" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat, #agility]
    , cdSlots = [#hand, #hand]
    , cdLevel = Just 2
    }

combatTraining3 :: CardDef
combatTraining3 =
  fast
    $ (asset "08027" "Combat Training" 0 Guardian)
      { cdSkills = [#combat, #combat, #agility, #agility]
      , cdCardTraits = setFromList [Talent, Composure]
      , cdLimits = [LimitPerTrait Composure 1]
      , cdLevel = Just 3
      }

butterflySwords5 :: CardDef
butterflySwords5 =
  (asset "08030" "Butterfly Swords" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat, #agility, #wild]
    , cdSlots = [#hand, #hand]
    , cdLevel = Just 5
    }

-- TODO: if we ever care about deck size need to encode that somehow
forcedLearning :: CardDef
forcedLearning =
  permanent
    $ (asset "08031" "Forced Learning" 0 Seeker)
      { cdCardTraits = setFromList [Talent, Ritual]
      , cdDeckRestrictions = [PerDeckLimit 1, PurchaseAtDeckCreation]
      }

jeremiahKirbyArcticArchaeologist :: CardDef
jeremiahKirbyArcticArchaeologist =
  (asset "08032" ("Jeremiah Kirby" <:> "Arctic Archaeologist") 4 Seeker)
    { cdCardTraits = setFromList [Ally, Miskatonic, Wayfarer]
    , cdSkills = [#intellect]
    , cdSlots = [#ally]
    , cdUnique = True
    }

archiveOfConduitsUnidentified :: CardDef
archiveOfConduitsUnidentified =
  (asset "08033" ("Archive of Conduits" <:> "Unidentified") 2 Seeker)
    { cdCardTraits = setFromList [Item, Tome, Occult]
    , cdSkills = [#wild]
    , cdSlots = [#hand]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdAdditionalCost = Just ArchiveOfConduitsUnidentifiedCost
    }

hikingBoots1 :: CardDef
hikingBoots1 =
  (asset "08035" "Hiking Boots" 2 Seeker)
    { cdCardTraits = setFromList [Item, Clothing, Footwear]
    , cdSkills = [#agility]
    , cdLimits = [LimitPerTrait Footwear 1]
    , cdLevel = Just 1
    }

medicalTexts2 :: CardDef
medicalTexts2 =
  (asset "08038" "Medical Texts" 2 Seeker)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

scientificTheory3 :: CardDef
scientificTheory3 =
  fast
    $ (asset "08040" "Scientific Theory" 0 Seeker)
      { cdSkills = [#intellect, #intellect, #combat, #combat]
      , cdCardTraits = setFromList [Talent, Composure]
      , cdLimits = [LimitPerTrait Composure 1]
      , cdLevel = Just 3
      }

archiveOfConduitsGatewayToTindalos4 :: CardDef
archiveOfConduitsGatewayToTindalos4 =
  (asset "08041" ("Archive of Conduits" <:> "Gateway to Tindalos") 4 Seeker)
    { cdCardTraits = setFromList [Ritual]
    , cdSkills = [#combat, #combat, #wild]
    , cdSlots = [#arcane]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdLevel = Just 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheGateway
    , cdUses = uses Leyline 4
    }

archiveOfConduitsGatewayToAcheron4 :: CardDef
archiveOfConduitsGatewayToAcheron4 =
  (asset "08042" ("Archive of Conduits" <:> "Gateway to Acheron") 4 Seeker)
    { cdCardTraits = setFromList [Ritual]
    , cdSkills = [#intellect, #intellect, #wild]
    , cdSlots = [#arcane]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdLevel = Just 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheGateway
    , cdUses = uses Leyline 4
    }

archiveOfConduitsGatewayToAldebaran4 :: CardDef
archiveOfConduitsGatewayToAldebaran4 =
  (asset "08043" ("Archive of Conduits" <:> "Gateway to Aldebaran") 4 Seeker)
    { cdCardTraits = setFromList [Ritual]
    , cdSkills = [#agility, #agility, #wild]
    , cdSlots = [#arcane]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdLevel = Just 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheGateway
    , cdUses = uses Leyline 4
    }

archiveOfConduitsGatewayToParadise4 :: CardDef
archiveOfConduitsGatewayToParadise4 =
  (asset "08044" ("Archive of Conduits" <:> "Gateway to Paradise") 4 Seeker)
    { cdCardTraits = setFromList [Ritual]
    , cdSkills = [#willpower, #willpower, #wild]
    , cdSlots = [#arcane]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdLevel = Just 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheGateway
    , cdUses = uses Leyline 4
    }

prophesiaeProfanaAtlasOfTheUnknowable5 :: CardDef
prophesiaeProfanaAtlasOfTheUnknowable5 =
  (asset "08045" ("Prophesiae Profana" <:> "Atlas of the Unknowable") 4 Seeker)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSlots = [#hand]
    , cdLevel = Just 5
    , cdUnique = True
    }

underworldSupport :: CardDef
underworldSupport =
  permanent
    $ (asset "08046" "Underworld Support" 0 Rogue)
      { cdCardTraits = setFromList [Favor, Illicit]
      , cdDeckRestrictions = [PerDeckLimit 1, PurchaseAtDeckCreation]
      }

theRedClockBrokenButReliable2 :: CardDef
theRedClockBrokenButReliable2 =
  (asset "08053" ("The Red Clock" <:> "Broken but Reliable") 2 Rogue)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#wild]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdExceptional = True
    , cdLevel = Just 2
    , cdUses = uses Charge 0
    }

moxie3 :: CardDef
moxie3 =
  fast
    $ (asset "08056" "Moxie" 0 Rogue)
      { cdSkills = [#willpower, #willpower, #agility, #agility]
      , cdCardTraits = setFromList [Talent, Composure]
      , cdLimits = [LimitPerTrait Composure 1]
      , cdLevel = Just 3
      }

theBlackFan3 :: CardDef
theBlackFan3 =
  (asset "08057" ("The Black Fan" <:> "Symbol of Power") 3 Rogue)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#intellect, #agility, #wild]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdExceptional = True
    , cdLevel = Just 3
    }

theRedClockBrokenButReliable5 :: CardDef
theRedClockBrokenButReliable5 =
  (asset "08058" ("The Red Clock" <:> "Broken but Reliable") 2 Rogue)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#wild, #wild]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdExceptional = True
    , cdLevel = Just 5
    , cdUses = uses Charge 0
    }

downTheRabbitHole :: CardDef
downTheRabbitHole =
  permanent
    $ (asset "08059" "Down the Rabbit Hole" 0 Mystic)
      { cdCardTraits = setFromList [Talent]
      , cdDeckRestrictions = [PerDeckLimit 1, PurchaseAtDeckCreation]
      }

dragonPole :: CardDef
dragonPole =
  (asset "08060" "Dragon Pole" 3 Mystic)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat]
    , cdSlots = [#hand, #hand]
    }

closeTheCircle1 :: CardDef
closeTheCircle1 =
  (asset "08062" "Close the Circle" 2 Mystic)
    { cdCardTraits = setFromList [Ritual, Synergy]
    , cdSkills = [#combat, #agility]
    , cdSlots = [#arcane]
    , cdLevel = Just 1
    , cdUses = Uses Charge (DifferentClassAmong $ oneOf [InPlayAreaOf You, IsThisCard])
    }

astronomicalAtlas3 :: CardDef
astronomicalAtlas3 =
  (asset "08067" "Astronomical Atlas" 3 Mystic)
    { cdSkills = [#willpower, #intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdLevel = Just 3
    }

healingWords3 :: CardDef
healingWords3 =
  (asset "08068" "Healing Words" 2 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    , cdLevel = Just 3
    }

grounded3 :: CardDef
grounded3 =
  fast
    $ (asset "08069" "Grounded" 0 Mystic)
      { cdSkills = [#wild, #wild]
      , cdCardTraits = setFromList [Talent, Composure]
      , cdLimits = [LimitPerTrait Composure 1]
      , cdLevel = Just 3
      }

trueMagickReworkingReality5 :: CardDef
trueMagickReworkingReality5 =
  (asset "08070" ("True Magick" <:> "Reworking Reality") 4 Mystic)
    { cdSkills = [#willpower, #willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdUses = uses Charge 1
    , cdLevel = Just 5
    , cdSlots = [#hand, #arcane]
    , cdUnique = True
    }

shortSupply :: CardDef
shortSupply =
  permanent
    $ (asset "08071" "Short Supply" 0 Survivor)
      { cdCardTraits = setFromList [Talent]
      , cdDeckRestrictions = [PerDeckLimit 1, PurchaseAtDeckCreation]
      }

schoffnersCatalogue :: CardDef
schoffnersCatalogue =
  (asset "08072" "Schoffner's Catalogue" 2 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 5
    }

bandages :: CardDef
bandages =
  (asset "08073" "Bandages" 2 Survivor)
    { cdCardTraits = setFromList [Item]
    , cdSkills = [#agility]
    , cdUses = uses Supply 3
    }

bangleOfJinxes1 :: CardDef
bangleOfJinxes1 =
  (asset "08075" "Bangle of Jinxes" 2 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdSlots = [#accessory]
    , cdUses = uses Charge 1
    , cdLevel = Just 1
    }

fireExtinguisher3 :: CardDef
fireExtinguisher3 =
  (asset "08080" "Fire Extinguisher" 2 Survivor)
    { cdCardTraits = setFromList [Item, Tool, Melee]
    , cdSkills = [#agility, #agility]
    , cdLevel = Just 3
    , cdSlots = [#hand]
    }

plucky3 :: CardDef
plucky3 =
  fast
    $ (asset "08081" "Plucky" 0 Survivor)
      { cdSkills = [#willpower, #willpower, #intellect, #intellect]
      , cdCardTraits = setFromList [Talent, Composure]
      , cdLimits = [LimitPerTrait Composure 1]
      , cdLevel = Just 3
      }

medicalStudent :: CardDef
medicalStudent =
  (multiClassAsset "08083" "Medical Student" 2 [Guardian, Seeker])
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic, Science]
    , cdSlots = [#ally]
    }

michaelLeigh5 :: CardDef
michaelLeigh5 =
  (multiClassAsset "08086" ("Michael Leigh" <:> "Experienced Hunter") 4 [Guardian, Seeker])
    { cdSkills = [#intellect, #combat, #wild]
    , cdCardTraits = setFromList [Ally, Detective]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Evidence 0
    , cdLevel = Just 5
    }

oldShotgun2 :: CardDef
oldShotgun2 =
  (multiClassAsset "08088" "Old Shotgun" 0 [Guardian, Rogue])
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSlots = [#hand, #hand]
    , cdUses = Uses Ammo (DuringEventCalculation (Fixed 2) (Fixed 0))
    , cdLevel = Just 2
    }

quickdrawHolster4 :: CardDef
quickdrawHolster4 =
  (multiClassAsset "08089" "Quickdraw Holster" 4 [Guardian, Rogue])
    { cdSkills = [#combat, #agility, #agility]
    , cdCardTraits = setFromList [Item, Tool, Illicit]
    , cdSlots = [#body]
    , cdLevel = Just 4
    }

brandOfCthugha1 :: CardDef
brandOfCthugha1 =
  (multiClassAsset "08090" "Brand of Cthugha" 2 [Guardian, Mystic])
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Spell]
    , cdLevel = Just 1
    , cdUses = uses Charge 6
    , cdSlots = [#arcane]
    }

nkosiMabatiEnigmaticWarlock3 :: CardDef
nkosiMabatiEnigmaticWarlock3 =
  (multiClassAsset "08091" ("Nkosi Mabati" <:> "Enigmatic Warlock") 4 [Guardian, Mystic])
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Ally, Sorcerer]
    , cdLevel = Just 3
    , cdSlots = [#ally]
    , cdUnique = True
    }

brandOfCthugha4 :: CardDef
brandOfCthugha4 =
  (multiClassAsset "08092" "Brand of Cthugha" 2 [Guardian, Mystic])
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell]
    , cdLevel = Just 4
    , cdUses = uses Charge 9
    , cdSlots = [#arcane]
    }

cyclopeanHammer5 :: CardDef
cyclopeanHammer5 =
  (multiClassAsset "08093" "Cyclopean Hammer" 5 [Guardian, Mystic])
    { cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Relic, Melee]
    , cdLevel = Just 5
    , cdSlots = [#hand, #hand]
    }

sledgehammer :: CardDef
sledgehammer =
  (multiClassAsset "08094" "Sledgehammer" 3 [Guardian, Survivor])
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdSlots = [#hand, #hand]
    }

protectiveGear2 :: CardDef
protectiveGear2 =
  (multiClassAsset "08095" "Protective Gear" 4 [Guardian, Survivor])
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Armor]
    , cdSlots = [#body]
    , cdLevel = Just 2
    }

sledgehammer4 :: CardDef
sledgehammer4 =
  (multiClassAsset "08096" "Sledgehammer" 3 [Guardian, Survivor])
    { cdSkills = [#combat, #combat, #combat]
    , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdSlots = [#hand, #hand]
    , cdLevel = Just 4
    }

pocketTelescope :: CardDef
pocketTelescope =
  (multiClassAsset "08097" "Pocket Telescope" 2 [Seeker, Rogue])
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    }

eonChart1 :: CardDef
eonChart1 =
  (multiClassAsset "08098" "Eon Chart" 2 [Seeker, Rogue])
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#intellect, #agility]
    , cdUses = uses Secret 3
    , cdLevel = Just 1
    , cdSlots = [#accessory]
    }

geneBeauregard3 :: CardDef
geneBeauregard3 =
  (multiClassAsset "08099" ("Gené Beauregard" <:> "Intrepid Explorer") 5 [Seeker, Rogue])
    { cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSkills = [#intellect, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 3
    }

eonChart4 :: CardDef
eonChart4 =
  (multiClassAsset "08100" "Eon Chart" 2 [Seeker, Rogue])
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#intellect, #agility, #wild]
    , cdUses = uses Secret 3
    , cdLevel = Just 4
    , cdSlots = [#accessory]
    }

divination1 :: CardDef
divination1 =
  (multiClassAsset "08101" "Divination" 3 [Seeker, Mystic])
    { cdCardTraits = setFromList [Spell, Augury]
    , cdSkills = [#intellect]
    , cdUses = uses Charge 4
    , cdLevel = Just 1
    , cdSlots = [#arcane]
    }

divination4 :: CardDef
divination4 =
  (multiClassAsset "08103" "Divination" 3 [Seeker, Mystic])
    { cdCardTraits = setFromList [Spell, Augury]
    , cdSkills = [#willpower, #intellect]
    , cdUses = uses Charge 6
    , cdLevel = Just 4
    , cdSlots = [#arcane]
    }

professorWilliamWebbFinderOfHiddenConnections :: CardDef
professorWilliamWebbFinderOfHiddenConnections =
  ( multiClassAsset
      "08104"
      ("Professor William Webb" <:> "Finder of Hidden Connections")
      3
      [Seeker, Survivor]
  )
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSkills = [#intellect]
    , cdUses = uses Secret 3
    , cdSlots = [#ally]
    , cdUnique = True
    }

icePick1 :: CardDef
icePick1 =
  fast
    (multiClassAsset "08105" "Ice Pick" 1 [Seeker, Survivor])
      { cdCardTraits = setFromList [Item, Melee, Tool]
      , cdSkills = [#combat]
      , cdSlots = [#hand]
      , cdLevel = Just 1
      }

professorWilliamWebbFinderOfHiddenConnections2 :: CardDef
professorWilliamWebbFinderOfHiddenConnections2 =
  ( multiClassAsset
      "08106"
      ("Professor William Webb" <:> "Finder of Hidden Connections")
      3
      [Seeker, Survivor]
  )
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSkills = [#intellect, #wild]
    , cdUses = uses Secret 3
    , cdLevel = Just 2
    , cdSlots = [#ally]
    , cdUnique = True
    }

icePick3 :: CardDef
icePick3 =
  fast
    (multiClassAsset "08107" "Ice Pick" 1 [Seeker, Survivor])
      { cdCardTraits = setFromList [Item, Melee, Tool]
      , cdSkills = [#intellect, #combat]
      , cdSlots = [#hand]
      , cdLevel = Just 3
      }

blur1 :: CardDef
blur1 =
  (multiClassAsset "08109" "Blur" 2 [Rogue, Mystic])
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#agility]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = Just 1
    }

blur4 :: CardDef
blur4 =
  (multiClassAsset "08111" "Blur" 2 [Rogue, Mystic])
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#willpower, #agility]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    , cdLevel = Just 4
    }

unscrupulousLoan3 :: CardDef
unscrupulousLoan3 =
  (multiClassAsset "08113" "Unscrupulous Loan" 0 [Rogue, Survivor])
    { cdCardTraits = setFromList [Pact]
    , cdLimits = [LimitPerInvestigator 1]
    , cdLevel = Just 3
    }

preciousMementoFromAFormerLife4 :: CardDef
preciousMementoFromAFormerLife4 =
  (multiClassAsset "08114" ("Precious Memento" <:> "From a Former Life") 3 [Rogue, Survivor])
    { cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdSkills = [#wild, #wild]
    , cdSlots = [#accessory]
    , cdLevel = Just 4
    }

preciousMementoFromAFutureLife4 :: CardDef
preciousMementoFromAFutureLife4 =
  (multiClassAsset "08115" ("Precious Memento" <:> "From a Future Life") 3 [Rogue, Survivor])
    { cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdSkills = [#wild, #wild]
    , cdSlots = [#accessory]
    , cdLevel = Just 4
    }

talismanOfProtection :: CardDef
talismanOfProtection =
  fast
    (multiClassAsset "08116" "Talisman of Protection" 2 [Mystic, Survivor])
      { cdCardTraits = setFromList [Item, Charm]
      , cdSkills = [#willpower]
      , cdSlots = [#arcane]
      }

earthlySerenity1 :: CardDef
earthlySerenity1 =
  (multiClassAsset "08117" "Earthly Serenity" 2 [Mystic, Survivor])
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#willpower]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    , cdLevel = Just 1
    }

enchantedBow2 :: CardDef
enchantedBow2 =
  (multiClassAsset "08118" "Enchanted Bow" 3 [Mystic, Survivor])
    { cdCardTraits = setFromList [Spell, Blessed, Weapon, Ranged]
    , cdSkills = [#willpower, #agility]
    , cdSlots = [#hand, #hand, #arcane]
    , cdUses = uses Charge 3
    , cdLevel = Just 2
    }

earthlySerenity4 :: CardDef
earthlySerenity4 =
  (multiClassAsset "08119" "Earthly Serenity" 2 [Mystic, Survivor])
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#willpower, #willpower]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 6
    , cdLevel = Just 4
    }

prophetic3 :: CardDef
prophetic3 =
  (multiClassAsset "08120" "Prophetic" 3 [Guardian, Mystic, Survivor])
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 3
    , cdUses = uses Resource 2
    }

sleuth3 :: CardDef
sleuth3 =
  (multiClassAsset "08121" "Sleuth" 3 [Guardian, Seeker, Mystic])
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 3
    , cdUses = uses Resource 2
    }

bruiser3 :: CardDef
bruiser3 =
  (multiClassAsset "08122" "Bruiser" 3 [Guardian, Rogue, Survivor])
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 3
    , cdUses = uses Resource 2
    }

crafty3 :: CardDef
crafty3 =
  (multiClassAsset "08123" "Crafty" 3 [Seeker, Rogue, Survivor])
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 3
    , cdUses = uses Resource 2
    }

antiquary3 :: CardDef
antiquary3 =
  (multiClassAsset "08124" "Antiquary" 3 [Seeker, Rogue, Mystic])
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 3
    , cdUses = uses Resource 2
    }

inTheThickOfIt :: CardDef
inTheThickOfIt =
  permanent
    $ (asset "08125" "In the Thick of It" 0 Neutral)
      { cdCardTraits = singleton Curse
      , cdPurchaseTrauma = PurchaseAnyTrauma 2
      }

heavyFurs :: CardDef
heavyFurs =
  (asset "08126" "Heavy Furs" 2 Neutral)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Armor]
    , cdSlots = [#body]
    }

sledDog :: CardDef
sledDog =
  (asset "08127" "Sled Dog" 3 Neutral)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Creature]
    , cdDeckRestrictions = [PerDeckLimit 4]
    , cdSlots = [#ally]
    }

rodOfAnimalism1 :: CardDef
rodOfAnimalism1 =
  (asset "08128" "Rod of Animalism" 2 Neutral)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    , cdLevel = Just 1
    , cdUnique = True
    }

professorWilliamDyerProfessorOfGeologyResolute :: CardDef
professorWilliamDyerProfessorOfGeologyResolute =
  (storyAsset "08587" ("Professor William Dyer" <:> "Professor of Geology") 1 FatalMirage)
    { cdCardTraits = setFromList [Ally, Miskatonic, Resolute]
    , cdCardType = AssetType
    , cdUses = uses Secret 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

danforthBrilliantStudentResolute :: CardDef
danforthBrilliantStudentResolute =
  (storyAsset "08588" ("Danforth" <:> "Brilliant Student") 1 FatalMirage)
    { cdCardTraits = setFromList [Ally, Miskatonic, Resolute]
    , cdCardType = AssetType
    , cdUses = uses Secret 5
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

eliyahAshevakDogHandlerResolute :: CardDef
eliyahAshevakDogHandlerResolute =
  (storyAsset "08589" ("Eliyah Ashevak" <:> "Dog Handler") 1 FatalMirage)
    { cdCardTraits = setFromList [Ally, Wayfarer, Resolute]
    , cdCardType = AssetType
    , cdUses = uses Secret 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

drMalaSinhaDaringPhysicianResolute :: CardDef
drMalaSinhaDaringPhysicianResolute =
  (storyAsset "08590" ("Dr. Mala Sinha" <:> "Daring Physician") 1 FatalMirage)
    { cdCardTraits = setFromList [Ally, Wayfarer, Resolute]
    , cdCardType = AssetType
    , cdUses = uses Supply 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

averyClaypoolAntarcticGuideResolute :: CardDef
averyClaypoolAntarcticGuideResolute =
  (storyAsset "08591" ("Avery Claypool" <:> "Antarctic Guide") 1 FatalMirage)
    { cdCardTraits = setFromList [Ally, Wayfarer, Resolute]
    , cdCardType = AssetType
    , cdUses = uses Supply 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

jamesCookieFredericksDubiousChoiceResolute :: CardDef
jamesCookieFredericksDubiousChoiceResolute =
  (storyAsset "08592" ("James \"Cookie\" Fredericks" <:> "Dubious Choice") 1 FatalMirage)
    { cdCardTraits = setFromList [Ally, Veteran, Wayfarer, Resolute]
    , cdCardType = AssetType
    , cdUses = uses Ammo 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

drAmyKenslerProfessorOfBiologyResolute :: CardDef
drAmyKenslerProfessorOfBiologyResolute =
  (storyAsset "08593" ("Dr. Amy Kensler" <:> "Professor of Biology") 1 FatalMirage)
    { cdCardTraits = setFromList [Ally, Miskatonic, Resolute]
    , cdCardType = AssetType
    , cdUses = uses Secret 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

roaldEllsworthIntrepidExplorerResolute :: CardDef
roaldEllsworthIntrepidExplorerResolute =
  (storyAsset "08594" ("Roald Ellsworth" <:> "Intrepid Explorer") 1 FatalMirage)
    { cdCardTraits = setFromList [Ally, Miskatonic, Wayfarer, Resolute]
    , cdCardType = AssetType
    , cdUses = uses Supply 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

takadaHirokoAeroplaneMechanicResolute :: CardDef
takadaHirokoAeroplaneMechanicResolute =
  (storyAsset "08595" ("Takada Hiroko" <:> "Aeroplane Mechanic") 1 FatalMirage)
    { cdCardTraits = setFromList [Ally, Wayfarer, Resolute]
    , cdCardType = AssetType
    , cdUses = uses Resource 9
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

greenSoapstoneJinxedIdol :: CardDef
greenSoapstoneJinxedIdol =
  (storyAsset "08614" ("Green Soapstone" <:> "Jinxed Idol") 1 ToTheForbiddenPeaks)
    { cdCardTraits = setFromList [Item, Trait.Expedition]
    , cdCardType = AssetType
    , cdUses = uses Charge 4
    , cdSkills = [#combat, #combat, #wild]
    }

woodenSledge :: CardDef
woodenSledge =
  (storyAsset "08615" "Wooden Sledge" 1 ToTheForbiddenPeaks)
    { cdCardTraits = setFromList [Item, Trait.Expedition]
    , cdCardType = AssetType
    , cdSkills = [#agility, #wild, #wild]
    }

dynamite :: CardDef
dynamite =
  (storyAsset "08616" "Dynamite" 1 ToTheForbiddenPeaks)
    { cdCardTraits = setFromList [Item, Trait.Expedition]
    , cdCardType = AssetType
    , cdUses = uses Supply 2
    , cdSkills = [#combat, #combat, #wild]
    }

miasmicCrystalStrangeEvidence :: CardDef
miasmicCrystalStrangeEvidence =
  (storyAsset "08617" ("Miasmic Crystal" <:> "Strange Evidence") 1 ToTheForbiddenPeaks)
    { cdCardTraits = setFromList [Item, Relic, Trait.Expedition]
    , cdCardType = AssetType
    , cdUses = uses Charge 3
    , cdSkills = [#willpower, #willpower, #wild]
    }

mineralSpecimen :: CardDef
mineralSpecimen =
  (storyAsset "08618" "Mineral Specimen" 1 ToTheForbiddenPeaks)
    { cdCardTraits = setFromList [Item, Relic, Trait.Expedition]
    , cdCardType = AssetType
    , cdUses = uses Charge 3
    , cdSkills = [#intellect, #intellect, #wild]
    }

smallRadio :: CardDef
smallRadio =
  (storyAsset "08619" "Small Radio" 1 ToTheForbiddenPeaks)
    { cdCardTraits = setFromList [Item, Trait.Expedition]
    , cdCardType = AssetType
    , cdUses = uses Supply 3
    , cdSkills = [#intellect, #intellect, #wild]
    }

spareParts :: CardDef
spareParts =
  (storyAsset "08620" "Spare Parts" 1 ToTheForbiddenPeaks)
    { cdCardTraits = setFromList [Item, Trait.Expedition]
    , cdCardType = AssetType
    , cdUses = uses Supply 3
    , cdSkills = [#willpower, #willpower, #wild]
    }

professorWilliamDyerProfessorOfGeology :: CardDef
professorWilliamDyerProfessorOfGeology =
  (storyAsset "08714" ("Professor William Dyer" <:> "Professor of Geology") 1 ExpeditionTeam)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdCardType = AssetType
    , cdUses = uses Secret 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

danforthBrilliantStudent :: CardDef
danforthBrilliantStudent =
  (storyAsset "08715" ("Danforth" <:> "Brilliant Student") 1 ExpeditionTeam)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdCardType = AssetType
    , cdUses = uses Secret 5
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

eliyahAshevakDogHandler :: CardDef
eliyahAshevakDogHandler =
  (storyAsset "08716" ("Eliyah Ashevak" <:> "Dog Handler") 1 ExpeditionTeam)
    { cdCardTraits = setFromList [Ally, Wayfarer]
    , cdCardType = AssetType
    , cdUses = uses Secret 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

drMalaSinhaDaringPhysician :: CardDef
drMalaSinhaDaringPhysician =
  (storyAsset "08717" ("Dr. Mala Sinha" <:> "Daring Physician") 1 ExpeditionTeam)
    { cdCardTraits = setFromList [Ally, Wayfarer]
    , cdCardType = AssetType
    , cdUses = uses Supply 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

averyClaypoolAntarcticGuide :: CardDef
averyClaypoolAntarcticGuide =
  (storyAsset "08718" ("Avery Claypool" <:> "Antarctic Guide") 1 ExpeditionTeam)
    { cdCardTraits = setFromList [Ally, Wayfarer]
    , cdCardType = AssetType
    , cdUses = uses Supply 5
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

jamesCookieFredericksDubiousChoice :: CardDef
jamesCookieFredericksDubiousChoice =
  (storyAsset "08719" ("James \"Cookie\" Fredericks" <:> "Dubious Choice") 1 ExpeditionTeam)
    { cdCardTraits = setFromList [Ally, Veteran, Wayfarer]
    , cdCardType = AssetType
    , cdUses = uses Ammo 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

drAmyKenslerProfessorOfBiology :: CardDef
drAmyKenslerProfessorOfBiology =
  (storyAsset "08720" ("Dr. Amy Kensler" <:> "Professor of Biology") 1 ExpeditionTeam)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdCardType = AssetType
    , cdUses = uses Secret 3
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

roaldEllsworthIntrepidExplorer :: CardDef
roaldEllsworthIntrepidExplorer =
  (storyAsset "08721" ("Roald Ellsworth" <:> "Intrepid Explorer") 1 ExpeditionTeam)
    { cdCardTraits = setFromList [Ally, Miskatonic, Wayfarer]
    , cdCardType = AssetType
    , cdUses = uses Supply 5
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

takadaHirokoAeroplaneMechanic :: CardDef
takadaHirokoAeroplaneMechanic =
  (storyAsset "08722" ("Takada Hiroko" <:> "Aeroplane Mechanic") 1 ExpeditionTeam)
    { cdCardTraits = setFromList [Ally, Wayfarer]
    , cdCardType = AssetType
    , cdUses = uses Resource 9
    , cdKeywords = singleton Keyword.Partner
    , cdUnique = True
    }

claypoolsFurs :: CardDef
claypoolsFurs =
  fast
    $ (asset "08730" "Claypool's Furs" 2 Neutral)
      { cdCardTraits = setFromList [Item, Clothing]
      , cdSlots = [#body]
      , cdSkills = [#combat, #wild]
      , cdEncounterSet = Just MemorialsOfTheLost
      , cdEncounterSetQuantity = Just 1
      }

collectedWorksOfPoe :: CardDef
collectedWorksOfPoe =
  (asset "08731" "Collected Works of Poe" 1 Neutral)
    { cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Charge 3
    , cdSkills = [#willpower, #wild]
    , cdEncounterSet = Just MemorialsOfTheLost
    , cdEncounterSetQuantity = Just 1
    }

cookiesCustom32 :: CardDef
cookiesCustom32 =
  fast
    $ (asset "08732" "Cookie's Custom .32" 2 Neutral)
      { cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdSlots = [#hand]
      , cdUses = uses Ammo 2
      , cdSkills = [#combat, #wild]
      , cdEncounterSet = Just MemorialsOfTheLost
      , cdEncounterSetQuantity = Just 1
      }

ellsworthsBoots :: CardDef
ellsworthsBoots =
  (asset "08734" "Ellsworth's Boots" 2 Neutral)
    { cdCardTraits = setFromList [Item, Clothing, Footwear]
    , cdSkills = [#agility, #wild]
    , cdLimits = [LimitPerTrait Footwear 1]
    , cdEncounterSet = Just MemorialsOfTheLost
    , cdEncounterSetQuantity = Just 1
    }

kenslersLog :: CardDef
kenslersLog =
  fast
    $ (asset "08735" "Kensler's Log" 2 Neutral)
      { cdCardTraits = setFromList [Item, Tome]
      , cdSkills = [#intellect, #wild]
      , cdSlots = [#hand]
      , cdEncounterSet = Just MemorialsOfTheLost
      , cdEncounterSetQuantity = Just 1
      }

sinhasMedicalKit :: CardDef
sinhasMedicalKit =
  fast
    $ (asset "08736" "Sinha's Medical Kit" 1 Neutral)
      { cdCardTraits = setFromList [Item, Science]
      , cdSkills = [#willpower, #wild]
      , cdUses = uses Supply 3
      , cdEncounterSet = Just MemorialsOfTheLost
      , cdEncounterSetQuantity = Just 1
      }

anyuFaithfulCompanion :: CardDef
anyuFaithfulCompanion =
  (asset "08738" ("Anyu" <:> "Faithful Companion") 3 Neutral)
    { cdCardTraits = setFromList [Ally, Creature]
    , cdSkills = [#agility, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdEncounterSet = Just MemorialsOfTheLost
    , cdEncounterSetQuantity = Just 1
    }
