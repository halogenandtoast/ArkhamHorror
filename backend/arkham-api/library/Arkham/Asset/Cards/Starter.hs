module Arkham.Asset.Cards.Starter where

import Arkham.Asset.Cards.Import
import Arkham.Asset.Uses qualified as Uses
import Arkham.Keyword qualified as Keyword

randallCho :: CardDef
randallCho =
  signature "60101"
    $ (asset "60102" ("Randall Cho" <:> "Concerned Brother") 2 Guardian)
      { cdSkills = [#willpower, #intellect, #wild]
      , cdCardTraits = setFromList [Ally, Medic]
      , cdUnique = True
      , cdSlots = [#ally]
      }

boxingGloves :: CardDef
boxingGloves =
  (asset "60105" "Boxing Gloves" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon]
    , cdSlots = [#hand, #hand]
    }

fleshWard :: CardDef
fleshWard =
  (asset "60106" "Flesh Ward" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Ritual
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    }

greteWagner :: CardDef
greteWagner =
  (asset "60107" ("Grete Wagner" <:> "The Purifier") 5 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Ally, Hunter]
    , cdSlots = [#ally]
    , cdUnique = True
    }

relentless :: CardDef
relentless =
  (asset "60109" "Relentless" 0 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Talent
    }

safeguard :: CardDef
safeguard =
  (asset "60110" "Safeguard" 2 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    }

boxingGloves3 :: CardDef
boxingGloves3 =
  (asset "60127" "Boxing Gloves" 2 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon]
    , cdSlots = [#hand, #hand]
    , cdLevel = Just 3
    }

greteWagner3 :: CardDef
greteWagner3 =
  (asset "60128" ("Grete Wagner" <:> "The Purifier") 5 Guardian)
    { cdSkills = [#combat, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Hunter]
    , cdSlots = [#ally]
    , cdLevel = Just 3
    , cdUnique = True
    }

physicalTraining4 :: CardDef
physicalTraining4 =
  (asset "60131" "Physical Training" 2 Guardian)
    { cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdCardTraits = setFromList [Talent]
    , cdUses = uses Resource 2
    , cdLevel = Just 4
    }

vaultOfKnowledge :: CardDef
vaultOfKnowledge =
  signature "60201"
    $ (asset "60202" "Vault of Knowledge" 3 Seeker)
      { cdSkills = [#willpower, #agility, #wild]
      , cdCardTraits = singleton Talent
      }

arcaneEnlightenment :: CardDef
arcaneEnlightenment =
  (asset "60205" "Arcane Enlightenment" 2 Seeker)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    }

celaenoFragments :: CardDef
celaenoFragments =
  (asset "60206" ("Celaeno Fragments" <:> "Book of Books") 1 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUnique = True
    , cdSlots = [#hand]
    }

discOfItzamna :: CardDef
discOfItzamna =
  (asset "60207" ("Disc of Itzamna" <:> "Protective Amulet") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    , cdSlots = [#accessory]
    }

encyclopedia :: CardDef
encyclopedia =
  (asset "60208" "Encyclopedia" 2 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 5
    , cdSlots = [#hand]
    }

feedTheMind :: CardDef
feedTheMind =
  (asset "60209" "Feed the Mind" 3 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Spell
    , cdUses = uses Secret 3
    , cdSlots = [#arcane]
    }

forbiddenTome :: CardDef
forbiddenTome =
  (asset "60210" ("Forbidden Tome" <:> "Untranslated") 1 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdUses = uses Secret 5
    , cdSlots = [#hand]
    }

higherEducation :: CardDef
higherEducation =
  (asset "60211" "Higher Education" 0 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Talent]
    }

whittonGreene :: CardDef
whittonGreene =
  (asset "60213" ("Whitton Greene" <:> "Hunter of Rare Books") 4 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

libraryDocent1 :: CardDef
libraryDocent1 =
  (asset "60220" "Library Docent" 1 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSlots = [#ally]
    , cdLevel = Just 1
    }

esotericAtlas2 :: CardDef
esotericAtlas2 =
  (asset "60222" "Esoteric Atlas" 3 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    , cdUses = uses Secret 4
    }

whittonGreene2 :: CardDef
whittonGreene2 =
  (asset "60223" ("Whitton Greene" <:> "Hunter of Rare Books") 4 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdLevel = Just 2
    }

forbiddenTomeDarkKnowledge3 :: CardDef
forbiddenTomeDarkKnowledge3 =
  (asset "60229" ("Forbidden Tome" <:> "Dark Knowledge") 1 Seeker)
    { cdSkills = [#willpower, #combat, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSlots = [#hand]
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheTome
    , cdLevel = Just 3
    }

forbiddenTomeSecretsRevealed3 :: CardDef
forbiddenTomeSecretsRevealed3 =
  (asset "60230" ("Forbidden Tome" <:> "Secrets Revealed") 1 Seeker)
    { cdSkills = [#intellect, #agility, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSlots = [#hand]
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheTome
    , cdLevel = Just 3
    }

farsight4 :: CardDef
farsight4 =
  (asset "60231" "Farsight" 2 Seeker)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = singleton Ritual
    , cdSlots = [#arcane]
    , cdLevel = Just 4
    }

miskatonicArchaeologyFunding4 :: CardDef
miskatonicArchaeologyFunding4 =
  permanent
    $ (asset "60232" "Miskatonic Archaeology Funding" 0 Seeker)
      { cdCardTraits = singleton Grant
      , cdLevel = Just 4
      }

theNecronomiconPetrusDeDaciaTranslation5 :: CardDef
theNecronomiconPetrusDeDaciaTranslation5 =
  (asset "60233" ("The Necronomicon" <:> "Petrus de Dacia Translation") 3 Seeker)
    { cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 6
    , cdSlots = [#hand]
    , cdLevel = Just 5
    , cdSkills = [#intellect, #intellect, #intellect, #intellect, #intellect]
    }

lockpicks :: CardDef
lockpicks =
  (asset "60305" "Lockpicks" 3 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool, Illicit]
    , cdSlots = [#hand]
    }

mauserC96 :: CardDef
mauserC96 =
  (asset "60306" "Mauser C96" 4 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 5
    }

lonnieRitter :: CardDef
lonnieRitter =
  (asset "60309" ("Lonnie Ritter" <:> "Feisty Mechanic") 4 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = singleton Ally
    , cdSlots = [#ally]
    , cdUnique = True
    }

leatherJacket :: CardDef
leatherJacket =
  fast
    $ (asset "60310" "Leather Jacket" 2 Rogue)
      { cdSkills = [#combat]
      , cdCardTraits = setFromList [Item, Armor]
      , cdSlots = [#body]
      }

streetwise :: CardDef
streetwise =
  (asset "60311" "Streetwise" 0 Rogue)
    { cdCardTraits = singleton Talent
    , cdSkills = [#intellect, #agility]
    }

liquidCourage1 :: CardDef
liquidCourage1 =
  (asset "60320" "Liquid Courage" 1 Rogue)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 4
    , cdLevel = Just 1
    }

mauserC962 :: CardDef
mauserC962 =
  (asset "60321" "Mauser C96" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 5
    , cdLevel = Just 2
    }

luckyCigaretteCase3 :: CardDef
luckyCigaretteCase3 =
  (asset "60326" "Lucky Cigarette Case" 2 Rogue)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdLevel = Just 3
    }

sharpshooter3 :: CardDef
sharpshooter3 =
  (asset "60327" "Sharpshooter" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Talent
    , cdLevel = Just 3
    }

berettaM19184 :: CardDef
berettaM19184 =
  (asset "60331" "Beretta M1918" 4 Rogue)
    { cdSkills = [#combat, #combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 4
    , cdSlots = [#hand, #hand]
    , cdLevel = Just 4
    }

chuckFergus5 :: CardDef
chuckFergus5 =
  (asset "60332" ("Chuck Fergus" <:> "O'Bannion Driver") 3 Rogue)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdSlots = [#ally]
    , cdLevel = Just 5
    , cdUnique = True
    }

arbiterOfFates :: CardDef
arbiterOfFates =
  signature "60401"
    $ (asset "60402" "Arbiter of Fates" 3 Mystic)
      { cdSkills = [#willpower, #agility, #wild]
      , cdCardTraits = singleton Talent
      }

scryingMirror :: CardDef
scryingMirror =
  (asset "60406" "Scrying Mirror" 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    }

azureFlame :: CardDef
azureFlame =
  (asset "60407" "Azure Flame" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    }

clairvoyance :: CardDef
clairvoyance =
  (asset "60408" "Clairvoyance" 4 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    }

ineffableTruth :: CardDef
ineffableTruth =
  (asset "60409" "Ineffable Truth" 3 Mystic)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    }

familiarSpirit :: CardDef
familiarSpirit =
  (asset "60410" "Familiar Spirit" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Creature, Summon]
    , cdSlots = [#ally]
    }

crystalPendulum :: CardDef
crystalPendulum =
  (asset "60411" "Crystal Pendulum" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    }

robesOfEndlessNight :: CardDef
robesOfEndlessNight =
  (asset "60412" "Robes of Endless Night" 3 Mystic)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Clothing]
    , cdSlots = [#body]
    }

grotesqueStatue2 :: CardDef
grotesqueStatue2 =
  (asset "60421" "Grotesque Statue" 3 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = Just 2
    , cdUses = uses Charge 3
    , cdSlots = [#hand]
    }

robesOfEndlessNight2 :: CardDef
robesOfEndlessNight2 =
  (asset "60422" "Robes of Endless Night" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Clothing]
    , cdSlots = [#body]
    , cdLevel = Just 2
    }

azureFlame3 :: CardDef
azureFlame3 =
  (asset "60425" "Azure Flame" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    , cdLevel = Just 3
    }

clairvoyance3 :: CardDef
clairvoyance3 =
  (asset "60426" "Clairvoyance" 4 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = Just 3
    }

ineffableTruth3 :: CardDef
ineffableTruth3 =
  (asset "60427" "Ineffable Truth" 3 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = Just 3
    }

arcaneStudies4 :: CardDef
arcaneStudies4 =
  (asset "60428" "Arcane Studies" 2 Mystic)
    { cdSkills = [#willpower, #willpower, #intellect, #intellect]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 4
    , cdUses = uses Resource 2
    }

azureFlame5 :: CardDef
azureFlame5 =
  (asset "60430" "Azure Flame" 3 Mystic)
    { cdSkills = [#willpower, #combat, #combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    , cdLevel = Just 5
    }

clairvoyance5 :: CardDef
clairvoyance5 =
  (asset "60431" "Clairvoyance" 4 Mystic)
    { cdSkills = [#willpower, #intellect, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = Just 5
    }

ineffableTruth5 :: CardDef
ineffableTruth5 =
  (asset "60432" "Ineffable Truth" 3 Mystic)
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = Just 5
    }

eighteenDerringer :: CardDef
eighteenDerringer =
  (asset "60505" ".18 Derringer" 3 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 2
    , cdSlots = [#hand]
    }

grimmsFairyTales :: CardDef
grimmsFairyTales =
  (asset "60506" "Grimm's Fairy Tales" 2 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 4
    , cdSlots = [#hand]
    }

oldKeyring :: CardDef
oldKeyring =
  (asset "60507" "Old Keyring" 1 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdUses = uses Uses.Key 2
    , cdSlots = [#hand]
    }

grannyOrne :: CardDef
grannyOrne =
  (asset "60508" ("Granny Orne" <:> "Tough Old Bird") 4 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Ally
    , cdSlots = [#ally]
    , cdUnique = True
    }

mysteriousRaven :: CardDef
mysteriousRaven =
  (asset "60509" "Mysterious Raven" 1 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Creature]
    , cdSlots = [#ally]
    }

scrapper :: CardDef
scrapper =
  (asset "60511" "Scrapper" 2 Survivor)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#combat, #agility]
    }

cherishedKeepsake1 :: CardDef
cherishedKeepsake1 =
  (asset "60520" "Cherished Keepsake" 0 Survivor)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdSkills = [#willpower]
    , cdLevel = Just 1
    }

leatherCoat1 :: CardDef
leatherCoat1 =
  (asset "60521" "Leather Coat" 0 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Armor]
    , cdSlots = [#body]
    , cdLevel = Just 1
    }

eighteenDerringer2 :: CardDef
eighteenDerringer2 =
  (asset "60522" ".18 Derringer" 2 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

grannyOrne3 :: CardDef
grannyOrne3 =
  (asset "60527" ("Granny Orne" <:> "Tough Old Bird") 4 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Ally
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 3
    }

chainsaw4 :: CardDef
chainsaw4 =
  (asset "60529" "Chainsaw" 4 Survivor)
    { cdSkills = [#combat, #combat, #combat]
    , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdUses = uses Supply 3
    , cdSlots = [#hand, #hand]
    , cdLevel = Just 4
    }

quickLearner4 :: CardDef
quickLearner4 =
  permanent
    $ (asset "60530" "Quick Learner" 0 Survivor)
      { cdCardTraits = singleton Condition
      , cdLevel = Just 4
      }

dejaVu5 :: CardDef
dejaVu5 =
  permanent
    $ (asset "60531" "Déjà Vu" 0 Survivor)
      { cdCardTraits = setFromList [Talent, Cursed]
      , cdLevel = Just 5
      }
