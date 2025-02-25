module Arkham.Asset.Cards.TheDrownedCity where

import Arkham.Asset.Cards.Import
import Arkham.Criteria qualified as Criteria
import Arkham.Keyword qualified as Keyword

bookOfVerseUnCommonplaceBook :: CardDef
bookOfVerseUnCommonplaceBook =
  signature "11004"
    $ (asset "11005" ("Book of Verse" <:> "Un-Commonplace Book") 2 Neutral)
      { cdCardTraits = setFromList [Item, Tome]
      , cdSkills = [#wild, #wild]
      , cdUnique = True
      , cdUses = uses Inspiration 1
      }

oculaObscuraEsotericEyepiece :: CardDef
oculaObscuraEsotericEyepiece =
  (asset "11009" ("Ocula Obscura" <:> "Esoteric Eyepiece") 3 Neutral)
    { cdCardTraits = setFromList [Item, Tool, Science]
    , cdSkills = [#willpower, #intellect, #wild]
    , cdUnique = True
    , cdDeckRestrictions = [Signature "11007", Signature "11008"]
    , cdSlots = [#accessory]
    }

violaCase :: CardDef
violaCase =
  signature "11011"
    $ (asset "11012" "\"Viola\" Case" 2 Neutral)
      { cdCardTraits = setFromList [Item, Illicit]
      , cdSkills = [#willpower, #combat, #agility, #wild]
      , cdSlots = [#accessory]
      }

theBookOfWarSunTzusLegacy :: CardDef
theBookOfWarSunTzusLegacy =
  (asset "11020" ("The Book of War" <:> "Sun Tzu's Legacy") 4 Guardian)
    { cdCardTraits = setFromList [Item, Tome]
    , cdSkills = [#intellect]
    , cdUnique = True
    , cdUses = uses Secret 3
    , cdSlots = [#hand]
    }

crowbar :: CardDef
crowbar =
  (asset "11021" "Crowbar" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Tool, Melee]
    , cdSkills = [#intellect, #combat]
    , cdSlots = [#hand]
    }

remingtonModel1858 :: CardDef
remingtonModel1858 =
  (asset "11022" "Remington Model 1858" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#agility]
    , cdUses = uses Ammo 2
    , cdSlots = [#hand]
    }

aliceLuxley2 :: CardDef
aliceLuxley2 =
  (asset "11027" ("Alice Luxley" <:> "Fearless Flatfoot") 4 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Detective, Police]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 2
    }

bulwark2 :: CardDef
bulwark2 =
  (asset "11028" "Bulwark" 2 Guardian)
    { cdSkills = [#willpower, #combat, #wild]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdLevel = Just 2
    , cdKeywords = setFromList [seal $ oneOf @ChaosTokenMatcher [#eldersign, #"+1"]]
    , cdLimits = [LimitPerInvestigator 1]
    }

lockedAndLoaded3 :: CardDef
lockedAndLoaded3 = permanent
  (asset "11030" "Locked and Loaded" 0 Guardian)
    { cdCardTraits = setFromList [Condition]
    , cdLevel = Just 3
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

remingtonModel18584 :: CardDef
remingtonModel18584 =
  (asset "11032" "Remington Model 1858" 2 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#combat, #combat, #agility, #agility]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand]
    , cdLevel = Just 4
    }

altonOConnellGhostHunter :: CardDef
altonOConnellGhostHunter =
  (asset "11033" ("Alton O'Connell" <:> "Ghost Hunter") 3 Seeker)
    { cdCardTraits = setFromList [Ally, Detective, Reporter]
    , cdSkills = [#intellect, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

artisticInspiration :: CardDef
artisticInspiration =
  (asset "11034" "Artistic Inspiration" 2 Seeker)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#wild]
    , cdLimits = [LimitPerInvestigator 1]
    , cdUses = uses Inspiration 1
    }

dialOfAncientsUnidentified :: CardDef
dialOfAncientsUnidentified =
  (asset "11035" ("Dial of Ancients" <:> "Unidentified") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSkills = [#intellect]
    , cdUses = uses Charge 0
    , cdSlots = [#accessory]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdUnique = True
    }

forensicKit :: CardDef
forensicKit =
  (asset "11036" "Forensic Kit" 3 Seeker)
    { cdCardTraits = setFromList [Item, Tool, Science]
    , cdSkills = [#intellect, #agility]
    , cdUses = uses Supply 4
    , cdSlots = [#hand]
    }

mortarAndPestle :: CardDef
mortarAndPestle =
  (asset "11037" "Mortar and Pestle" 2 Seeker)
    { cdCardTraits = setFromList [Item, Tool, Science]
    , cdSkills = [#willpower]
    , cdUses = uses Resource 0
    }

oculusMortuum :: CardDef
oculusMortuum =
  (asset "11038" "Oculus Mortuum" 3 Seeker)
    { cdCardTraits = setFromList [Item, Tool, Occult]
    , cdUses = uses Evidence 0
    , cdSlots = [#hand]
    , cdUnique = True
    }

uncannySpecimen :: CardDef
uncannySpecimen =
  fast
    $ (asset "11039" "Uncanny Specimen" 1 Seeker)
      { cdSlots = [#arcane]
      , cdCardTraits = setFromList [Creature, Science]
      , cdKeywords = setFromList [Keyword.Myriad]
      }

misdirection2 :: CardDef
misdirection2 =
  (asset "11043" "Misdirection" 2 Seeker)
    { cdSlots = [#arcane]
    , cdCardTraits = setFromList [Ritual]
    , cdLimits = [LimitPerInvestigator 1]
    , cdKeywords = setFromList [seal $ oneOf @ChaosTokenMatcher [#"0", #"+1"]]
    }

dialOfAncientsSignsOfCataclysm4 :: CardDef
dialOfAncientsSignsOfCataclysm4 =
  (asset "11046" ("Dial of Ancients" <:> "Signs of Cataclysm") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSkills = [#intellect, #wild]
    , cdUses = uses Charge 4
    , cdSlots = [#accessory]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdKeywords = setFromList [Keyword.Researched YouHaveCalculatedTheDayOfReckoning]
    , cdUnique = True
    , cdLevel = Just 4
    }

dialOfAncientsSignsOfAberration4 :: CardDef
dialOfAncientsSignsOfAberration4 =
  (asset "11047" ("Dial of Ancients" <:> "Signs of Aberration") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSkills = [#combat, #wild]
    , cdUses = uses Charge 4
    , cdSlots = [#accessory]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdKeywords = setFromList [Keyword.Researched YouHaveCalculatedTheDayOfReckoning]
    , cdUnique = True
    , cdLevel = Just 4
    }

dialOfAncientsSignsOfRevelation4 :: CardDef
dialOfAncientsSignsOfRevelation4 =
  (asset "11048" ("Dial of Ancients" <:> "Signs of Revelation") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSkills = [#willpower, #wild]
    , cdUses = uses Charge 4
    , cdSlots = [#accessory]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdKeywords = setFromList [Keyword.Researched YouHaveCalculatedTheDayOfReckoning]
    , cdUnique = True
    , cdLevel = Just 4
    }

lugerP08 :: CardDef
lugerP08 =
  (asset "11050" "Luger P08" 2 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 2
    }

robertCastaigneHasYourBack :: CardDef
robertCastaigneHasYourBack =
  (asset "11051" ("Robert Castaigne" <:> "Has Your Back") 4 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Veteran]
    , cdSlots = [#ally]
    , cdUnique = True
    }

stringAlong :: CardDef
stringAlong =
  (asset "11052" "String Along" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Talent]
    }

bloodOfThothLawIncarnate :: CardDef
bloodOfThothLawIncarnate =
  (asset "11065" ("Blood of Thoth" <:> "Law Incarnate") 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    , cdSlots = [#arcane]
    }

breathOfTheSleeper :: CardDef
breathOfTheSleeper =
  (asset "11066" "Breath of the Sleeper" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 9
    , cdSlots = [#arcane]
    }

eyesOfTheDreamer :: CardDef
eyesOfTheDreamer =
  (asset "11067" "Eyes of the Dreamer" 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 9
    , cdSlots = [#arcane]
    }

theGreatWorkDivideAndUnite :: CardDef
theGreatWorkDivideAndUnite =
  permanent
    $ (asset "11068a" ("The Great Work" <:> "Divide and Unite") 0 Mystic)
      { cdCardTraits = setFromList [Pact, Science]
      , cdUnique = True
      , cdDeckRestrictions = [PerDeckLimit 1]
      }

katarinaSojkamissaryFromUlthar :: CardDef
katarinaSojkamissaryFromUlthar =
  (asset "11069" ("Katarina Sojka" <:> "Emissary from Ulthar") 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Clairvoyant, Dreamer]
    , cdSlots = [#ally]
    , cdUnique = True
    }

signOfXelotaphSymbolOfProtection :: CardDef
signOfXelotaphSymbolOfProtection =
  (asset "11070" ("Sign of Xelotaph" <:> "Symbol of Protection") 3 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdKeywords =
        setFromList
          [Keyword.Myriad, seal $ oneOf @ChaosTokenMatcher [#skull, #cultist, #tablet, #elderthing]]
    , cdUses = uses Charge 3
    }

anchorChain :: CardDef
anchorChain =
  (asset "11081" "Anchor Chain" 3 Survivor)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    , cdCardInHandEffects = True
    }

handHook :: CardDef
handHook =
  (asset "11082" "Hand Hook" 3 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Tool, Melee]
    , cdSlots = [#hand]
    , cdCardInHandEffects = True
    }

lawrenceCarlisleSculptingHisDreams :: CardDef
lawrenceCarlisleSculptingHisDreams =
  (asset "11083" ("Lawrence Carlisle" <:> "Sculpting His Dreams") 3 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Artist, Patron]
    , cdSlots = [#ally]
    , cdUnique = True
    }

nauticalCharts :: CardDef
nauticalCharts =
  (asset "11084" "Nautical Charts" 3 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome, Tool]
    , cdSlots = [#hand]
    , cdCardInHandEffects = True
    }

profaneIdol :: CardDef
profaneIdol =
  (asset "11085" "Profane Idol" 1 Survivor)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdCriteria = Just Criteria.InYourDiscard
    , cdPlayableFromDiscard = True
    }

ascetic :: CardDef
ascetic =
  permanent
    (asset "11095" "Ascetic" 0 Neutral)
      { cdCardTraits = setFromList [Condition]
      , cdDeckRestrictions = [PurchaseAtDeckCreation, PerDeckLimit 1]
      , cdGrantedXp = Just 10
      }
