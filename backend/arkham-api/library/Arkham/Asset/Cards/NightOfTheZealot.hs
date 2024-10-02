module Arkham.Asset.Cards.NightOfTheZealot where

import Arkham.Asset.Cards.Import

rolands38Special :: CardDef
rolands38Special =
  signature "01001"
    $ (asset "01006" "Roland's .38 Special" 3 Neutral)
      { cdSkills = [#combat, #agility, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdUnique = True
      , cdUses = uses Ammo 4
      , cdSlots = [#hand]
      , cdAlternateCardCodes = ["01506"]
      }

daisysToteBag :: CardDef
daisysToteBag =
  signature "01002"
    $ (asset "01008" "Daisy's Tote Bag" 2 Neutral)
      { cdSkills = [#willpower, #intellect, #wild]
      , cdCardTraits = setFromList [Item]
      , cdUnique = True
      , cdAlternateCardCodes = ["01508"]
      }

theNecronomicon :: CardDef
theNecronomicon =
  (weakness "01009" ("The Necronomicon" <:> "John Dee Translation"))
    { cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01509"]
    }

heirloomOfHyperborea :: CardDef
heirloomOfHyperborea =
  signature "01004"
    $ (asset "01012" ("Heirloom of Hyperborea" <:> "Artifact from Another Life") 3 Neutral)
      { cdSkills = [#willpower, #combat, #wild]
      , cdCardTraits = setFromList [Item, Relic]
      , cdUnique = True
      , cdSlots = [#accessory]
      , cdAlternateCardCodes = ["01512"]
      }

wendysAmulet :: CardDef
wendysAmulet =
  signature "01005"
    $ (asset "01014" "Wendy's Amulet" 2 Neutral)
      { cdSkills = [#wild, #wild]
      , cdCardTraits = setFromList [Item, Relic]
      , cdUnique = True
      , cdSlots = [#accessory]
      , cdAlternateCardCodes = ["01514"]
      }

fortyFiveAutomatic :: CardDef
fortyFiveAutomatic =
  (asset "01016" ".45 Automatic" 4 Guardian)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUses = uses Ammo 4
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01516"]
    }

physicalTraining :: CardDef
physicalTraining =
  (asset "01017" "Physical Training" 2 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01517", "60108"]
    }

beatCop :: CardDef
beatCop =
  (asset "01018" "Beat Cop" 4 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Police]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01518"]
    }

firstAid :: CardDef
firstAid =
  (asset "01019" "First Aid" 2 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Talent, Science]
    , cdUses = uses Supply 3
    , cdAlternateCardCodes = ["01519"]
    }

machete :: CardDef
machete =
  (asset "01020" "Machete" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01520"]
    }

guardDog :: CardDef
guardDog =
  (asset "01021" "Guard Dog" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Creature]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01521"]
    }

policeBadge2 :: CardDef
policeBadge2 =
  (asset "01027" "Police Badge" 3 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item]
    , cdLevel = Just 2
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01527"]
    }

beatCop2 :: CardDef
beatCop2 =
  (asset "01028" "Beat Cop" 4 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Ally, Police]
    , cdLevel = Just 2
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01528"]
    }

shotgun4 :: CardDef
shotgun4 =
  (asset "01029" "Shotgun" 5 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdLevel = Just 4
    , cdUses = uses Ammo 2
    , cdSlots = [#hand, #hand]
    , cdAlternateCardCodes = ["01529"]
    }

magnifyingGlass :: CardDef
magnifyingGlass =
  fast
    $ (asset "01030" "Magnifying Glass" 1 Seeker)
      { cdSkills = [#intellect]
      , cdCardTraits = setFromList [Item, Tool]
      , cdSlots = [#hand]
      , cdAlternateCardCodes = ["01530"]
      }

oldBookOfLore :: CardDef
oldBookOfLore =
  (asset "01031" "Old Book of Lore" 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01531"]
    }

researchLibrarian :: CardDef
researchLibrarian =
  (asset "01032" "Research Librarian" 2 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01532"]
    }

drMilanChristopher :: CardDef
drMilanChristopher =
  (asset "01033" ("Dr. Milan Christopher" <:> "Professor of Entomology") 4 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01533"]
    }

hyperawareness :: CardDef
hyperawareness =
  (asset "01034" "Hyperawareness" 2 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01534"]
    }

medicalTexts :: CardDef
medicalTexts =
  (asset "01035" "Medical Texts" 2 Seeker)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01535"]
    }

magnifyingGlass1 :: CardDef
magnifyingGlass1 =
  fast
    $ (asset "01040" "Magnifying Glass" 0 Seeker)
      { cdSkills = [#intellect]
      , cdCardTraits = setFromList [Item, Tool]
      , cdLevel = Just 1
      , cdSlots = [#hand]
      , cdAlternateCardCodes = ["01540"]
      }

discOfItzamna2 :: CardDef
discOfItzamna2 =
  (asset "01041" ("Disc of Itzamna" <:> "Protective Amulet") 3 Seeker)
    { cdSkills = [#willpower, #intellect, #combat]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = Just 2
    , cdUnique = True
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01541"]
    }

encyclopedia2 :: CardDef
encyclopedia2 =
  (asset "01042" "Encyclopedia" 2 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Tome]
    , cdLevel = Just 2
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01542"]
    }

switchblade :: CardDef
switchblade =
  fast
    $ (asset "01044" "Switchblade" 1 Rogue)
      { cdSkills = [#agility]
      , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
      , cdSlots = [#hand]
      , cdAlternateCardCodes = ["01544", "60307"]
      }

burglary :: CardDef
burglary =
  (asset "01045" "Burglary" 1 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Talent, Illicit]
    , cdAlternateCardCodes = ["01545"]
    }

pickpocketing :: CardDef
pickpocketing =
  (asset "01046" "Pickpocketing" 2 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Talent, Illicit]
    , cdAlternateCardCodes = ["01546"]
    }

fortyOneDerringer :: CardDef
fortyOneDerringer =
  (asset "01047" ".41 Derringer" 3 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01547"]
    }

leoDeLuca :: CardDef
leoDeLuca =
  (asset "01048" ("Leo De Luca" <:> "The Louisiana Lion") 6 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01548"]
    }

hardKnocks :: CardDef
hardKnocks =
  (asset "01049" "Hard Knocks" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01549"]
    }

leoDeLuca1 :: CardDef
leoDeLuca1 =
  (asset "01054" ("Leo De Luca" <:> "The Louisiana Lion") 5 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdLevel = Just 1
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01554"]
    }

catBurglar1 :: CardDef
catBurglar1 =
  (asset "01055" "Cat Burglar" 4 Rogue)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdLevel = Just 1
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01555"]
    }

forbiddenKnowledge :: CardDef
forbiddenKnowledge =
  (asset "01058" "Forbidden Knowledge" 0 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Talent]
    , cdUses = uses Secret 4
    , cdAlternateCardCodes = ["01558"]
    }

holyRosary :: CardDef
holyRosary =
  (asset "01059" "Holy Rosary" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01559"]
    }

shrivelling :: CardDef
shrivelling =
  (asset "01060" "Shrivelling" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    , cdAlternateCardCodes = ["01560"]
    }

scrying :: CardDef
scrying =
  (asset "01061" "Scrying" 1 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdAlternateCardCodes = ["01561"]
    }

arcaneStudies :: CardDef
arcaneStudies =
  (asset "01062" "Arcane Studies" 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01562"]
    }

arcaneInitiate :: CardDef
arcaneInitiate =
  (asset "01063" "Arcane Initiate" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Sorcerer]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01563"]
    }

bookOfShadows3 :: CardDef
bookOfShadows3 =
  (asset "01070" "Book of Shadows" 4 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdLevel = Just 3
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01570"]
    }

grotesqueStatue4 :: CardDef
grotesqueStatue4 =
  (asset "01071" "Grotesque Statue" 2 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = Just 4
    , cdUses = uses Charge 4
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01571"]
    }

leatherCoat :: CardDef
leatherCoat =
  (asset "01072" "Leather Coat" 0 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Armor]
    , cdSlots = [#body]
    , cdAlternateCardCodes = ["01572"]
    }

scavenging :: CardDef
scavenging =
  (asset "01073" "Scavenging" 1 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01573"]
    }

baseballBat :: CardDef
baseballBat =
  (asset "01074" "Baseball Bat" 2 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand, #hand]
    , cdAlternateCardCodes = ["01574"]
    }

rabbitsFoot :: CardDef
rabbitsFoot =
  (asset "01075" "Rabbit's Foot" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01575", "60510"]
    }

strayCat :: CardDef
strayCat =
  (asset "01076" "Stray Cat" 1 Survivor)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Ally, Creature]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01576"]
    }

digDeep :: CardDef
digDeep =
  (asset "01077" "Dig Deep" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01577"]
    }

aquinnah1 :: CardDef
aquinnah1 =
  (asset "01082" ("Aquinnah" <:> "The Forgotten Daughter") 5 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally]
    , cdLevel = Just 1
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01582"]
    }

knife :: CardDef
knife =
  (asset "01086" "Knife" 1 Neutral)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01586"]
    }

flashlight :: CardDef
flashlight =
  (asset "01087" "Flashlight" 2 Neutral)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdUses = uses Supply 3
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01587"]
    }

bulletproofVest3 :: CardDef
bulletproofVest3 =
  (asset "01094" "Bulletproof Vest" 2 Neutral)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = setFromList [Item, Armor]
    , cdLevel = Just 3
    , cdSlots = [#body]
    , cdAlternateCardCodes = ["01594"]
    }

elderSignAmulet3 :: CardDef
elderSignAmulet3 =
  (asset "01095" "Elder Sign Amulet" 2 Neutral)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = Just 3
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01595"]
    }

litaChantler :: CardDef
litaChantler =
  (storyAsset "01117" ("Lita Chantler" <:> "The Zealot") 0 TheGathering)
    { cdCardTraits = setFromList [Ally]
    , cdUnique = True
    , cdSlots = [#ally]
    }
