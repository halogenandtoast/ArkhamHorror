module Arkham.Asset.Cards.TheDrownedCity where

import Arkham.Asset.Cards.Import
import Arkham.Criteria qualified as Criteria
import Arkham.Keyword qualified as Keyword
import Arkham.Token qualified as Token
import Arkham.Trait qualified as Trait

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
    , cdLevel = Nothing
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
    { cdSkills = [#intellect, #intellect]
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
lockedAndLoaded3 =
  permanent
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
    { cdSkills = [#intellect, #agility, #wild]
    , cdSlots = [#arcane]
    , cdCardTraits = setFromList [Ritual]
    , cdLimits = [LimitPerInvestigator 1]
    , cdKeywords = setFromList [seal $ oneOf @ChaosTokenMatcher [#"0", #"+1"]]
    , cdLevel = Just 2
    }

scientificStudiesGrant2 :: CardDef
scientificStudiesGrant2 =
  (asset "11044" "Scientific Studies Grant" 3 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Grant]
    , cdLimits = [LimitPerInvestigator 1]
    , cdLevel = Just 2
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

antikytheraPropheticTimepiece5 :: CardDef
antikytheraPropheticTimepiece5 =
  (asset "11049" ("Antikythera" <:> "Prophetic Timepiece") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Science]
    , cdSkills = [#intellect, #wild, #wild]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdLevel = Just 5
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

mobConnections2 :: CardDef
mobConnections2 =
  (asset "11059" "Mob Connections" 1 Rogue)
    { cdCardTraits = setFromList [Connection, Illicit]
    , cdKeywords = setFromList [Keyword.Exceptional]
    , cdLevel = Just 2
    }

obscure2 :: CardDef
obscure2 =
  (asset "11060" "Obscure" 2 Rogue)
    { cdSkills = [#intellect, #agility, #wild]
    , cdCardTraits = setFromList [Ritual]
    , cdLimits = [LimitPerInvestigator 1]
    , cdKeywords = setFromList [seal $ chaosToken_ #"0"]
    , cdLevel = Just 2
    }

robertCastaigneStillHasYourBack4 :: CardDef
robertCastaigneStillHasYourBack4 =
  (asset "11062" ("Robert Castaigne" <:> "Still Has Your Back") 3 Rogue)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Ally, Veteran]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 4
    }

gatlingGun5 :: CardDef
gatlingGun5 =
  (asset "11064" "Gatling Gun" 5 Rogue)
    { cdSkills = [#combat, #combat, #agility, #wild]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 12
    , cdLevel = Just 5
    }

bloodOfThothLawIncarnate :: CardDef
bloodOfThothLawIncarnate =
  (asset "11065" ("Blood of Thoth" <:> "Law Incarnate") 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    , cdSlots = [#accessory]
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
      , cdDoubleSided = True
      , cdOtherSide = Just "11068b"
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

eyeOfGhatanothoaArtifactOfTheDarkGod2 :: CardDef
eyeOfGhatanothoaArtifactOfTheDarkGod2 =
  (asset "11073" ("Eye of Ghatanothoa" <:> "Artifact of the Dark God") 2 Mystic)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdKeywords = setFromList [Keyword.Exceptional]
    , cdLevel = Just 2
    }

swordCaneDesignedByTheCouncilOfPolls2 :: CardDef
swordCaneDesignedByTheCouncilOfPolls2 =
  fast
    (asset "11074" ("Sword Cane" <:> "Designed by the Council of Polls") 2 Mystic)
      { cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
      , cdSkills = [#combat, #agility]
      , cdSlots = [#hand]
      , cdLevel = Just 2
      }

lostArcana3 :: CardDef
lostArcana3 =
  (asset "11078" "Lost Arcana" 3 Mystic)
    { cdCardTraits = setFromList [Item, Tome, Occult]
    , cdSkills = [#willpower, #intellect]
    , cdSlots = [#hand]
    , cdUses = uses Charge 2
    , cdLevel = Just 3
    }

eldritchBrand5 :: CardDef
eldritchBrand5 =
  permanent
    (asset "11080" "Eldritch Brand" 0 Mystic)
      { cdCardTraits = setFromList [Pact]
      , cdExceptional = True
      , cdLevel = Just 5
      }

anchorChain :: CardDef
anchorChain =
  (asset "11081" "Anchor Chain" 3 Survivor)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    , cdOutOfPlayEffects = [InHandEffect]
    }

handHook :: CardDef
handHook =
  (asset "11082" "Hand Hook" 3 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Tool, Melee]
    , cdSlots = [#hand]
    , cdOutOfPlayEffects = [InHandEffect]
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
    , cdOutOfPlayEffects = [InHandEffect]
    }

profaneIdol :: CardDef
profaneIdol =
  (asset "11085" "Profane Idol" 1 Survivor)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdCriteria = Just Criteria.InYourDiscard
    , cdPlayableFromDiscard = True
    }

ampleSupplies2 :: CardDef
ampleSupplies2 =
  (asset "11089" "Ample Supplies" 1 Survivor)
    { cdCardTraits = setFromList [Trait.Supply]
    , cdUses = uses Supply 2
    , cdLimits = [LimitPerInvestigator 1]
    , cdLevel = Just 2
    }

giftOfNodens5 :: CardDef
giftOfNodens5 =
  (asset "11094" "Gift of Nodens" 2 Survivor)
    { cdCardTraits = setFromList [Ritual]
    , cdSkills = [#willpower]
    , cdLevel = Just 5
    , cdSlots = [#arcane]
    , cdUnique = True
    }

ascetic :: CardDef
ascetic =
  permanent
    (asset "11095" "Ascetic" 0 Neutral)
      { cdCardTraits = setFromList [Condition]
      , cdDeckRestrictions = [PurchaseAtDeckCreation, PerDeckLimit 1]
      , cdGrantedXp = Just 10
      }

spiritualHealing4 :: CardDef
spiritualHealing4 =
  permanent
    $ (asset "11098" "Spiritual Healing" 0 Neutral)
      { cdCardTraits = setFromList [Condition, Blessed]
      , cdLevel = Just 4
      , cdDeckRestrictions = [PerDeckLimit 1]
      }

libraryPass1 :: CardDef
libraryPass1 =
  (asset "11099" "Library Pass" 2 Neutral)
    { cdCardTraits = setFromList [Item, Miskatonic]
    , cdLevel = Just 1
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Miskatonic, Scholar], PerDeckLimit 1]
    }

boundForTheHorizon2 :: CardDef
boundForTheHorizon2 =
  (asset "11102" "Bound for the Horizon" 2 Neutral)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#agility, #agility]
    , cdLevel = Just 2
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Drifter, Hunter, Wayfarer]]
    , cdLimits = [LimitPerInvestigator 1]
    }

forbiddenSutra2 :: CardDef
forbiddenSutra2 =
  (asset "11103" "Forbidden Sutra" 2 Neutral)
    { cdCardTraits = setFromList [Item, Tome, Blessed, Cursed]
    , cdSkills = [#willpower, #wild]
    , cdLevel = Just 2
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Blessed, Cursed, Sorcerer]]
    , cdSlots = [#hand]
    }

walterFitzpatrickPlayingBothSides2 :: CardDef
walterFitzpatrickPlayingBothSides2 =
  (asset "11104" ("Walter Fitzpatrick" <:> "Playing Both Sides") 3 Neutral)
    { cdCardTraits = setFromList [Ally, Patron, SilverTwilight]
    , cdSkills = [#willpower, #intellect]
    , cdLevel = Just 2
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Civic, SilverTwilight, Socialite]]
    , cdSlots = [#ally]
    , cdUnique = True
    }

captivatingPerformance3 :: CardDef
captivatingPerformance3 =
  (asset "11108" "Captivating Performance" 3 Neutral)
    { cdCardTraits = setFromList [Ritual]
    , cdSkills = [#willpower, #wild]
    , cdLevel = Just 3
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Dreamer, Performer, Sorcerer]]
    , cdLimits = [LimitPerInvestigator 1]
    }

cowlOfSekhmetCloakOfPharaohs3 :: CardDef
cowlOfSekhmetCloakOfPharaohs3 =
  (asset "11109" ("Cowl of Sekhmet" <:> "Cloak of Pharaohs") 2 Neutral)
    { cdCardTraits = setFromList [Item, Clothing, Relic]
    , cdSkills = [#willpower]
    , cdLevel = Just 3
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Believer, Chosen, Cultist]]
    , cdSlots = [#body]
    , cdUnique = True
    }

dakotaGarofaloOnTheHunt3 :: CardDef
dakotaGarofaloOnTheHunt3 =
  (asset "11110" ("Dakota Garofalo" <:> "On the Hunt") 4 Neutral)
    { cdCardTraits = setFromList [Ally, Hunter, Wayfarer]
    , cdSkills = [#willpower, #combat]
    , cdLevel = Just 3
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Detective, Hunter, Wayfarer]]
    , cdSlots = [#ally]
    , cdUnique = True
    }

noseToTheGrindstone3 :: CardDef
noseToTheGrindstone3 =
  (asset "11111" "Nose to the Grindstone" 2 Neutral)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#intellect, #intellect]
    , cdLevel = Just 3
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Criminal, Drifter, Entrepreneur]]
    }

soundSupport3 :: CardDef
soundSupport3 =
  (asset "11112" "Sound Support" 2 Neutral)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#willpower, #willpower]
    , cdLevel = Just 3
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Assistant, Medic, Warden]]
    , cdLimits = [LimitPerInvestigator 1]
    }

trenchArmor3 :: CardDef
trenchArmor3 =
  (asset "11113" "Trench Armor" 2 Neutral)
    { cdCardTraits = setFromList [Item, Armor]
    , cdSkills = [#willpower, #combat, #combat]
    , cdLevel = Just 3
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Agency, Police, Veteran]]
    , cdSlots = [#body]
    }

stormRuler4 :: CardDef
stormRuler4 =
  (asset "11115" "Storm Ruler" 3 Neutral)
    { cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdSkills = [#willpower, #combat]
    , cdLevel = Just 4
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Blessed, Clairvoyant, Sorcerer]]
    , cdSlots = [#hand]
    , cdUnique = True
    }

archibaldMacVeighBeleaguredLecturer5 :: CardDef
archibaldMacVeighBeleaguredLecturer5 =
  (asset "11117" ("Archibald MacVeigh" <:> "Beleagured Lecturer") 4 Neutral)
    { cdCardTraits = setFromList [Ally]
    , cdSkills = [#intellect, #wild]
    , cdLevel = Just 5
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Assistant, Reporter, Scholar]]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Secret 4
    }

libraryPass5 :: CardDef
libraryPass5 =
  permanent
    (asset "11118" "Library Pass" 0 Neutral)
      { cdCardTraits = setFromList [Item, Miskatonic]
      , cdLevel = Just 5
      , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Miskatonic, Scholar], PerDeckLimit 1]
      }

mauserTankgewehrM19185 :: CardDef
mauserTankgewehrM19185 =
  (asset "11119" "Mauser Tankgewehr M1918" 4 Neutral)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#combat, #combat]
    , cdLevel = Just 5
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Hunter, Veteran, Warden]]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Shell 1
    }

onTheirHeels5 :: CardDef
onTheirHeels5 =
  (asset "11120" "On Their Heels" 2 Neutral)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#intellect, #combat, #wild]
    , cdLevel = Just 5
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Detective, Hunter, Police, Reporter]]
    , cdUses = uses Token.Lead 5
    , cdLimits = [LimitPerInvestigator 1]
    }

sacredOathOathOfLoyalty5 :: CardDef
sacredOathOathOfLoyalty5 =
  fast
    (asset "11121" ("Sacred Oath" <:> "Oath of Loyalty") 1 Neutral)
      { cdCardTraits = setFromList [Pact]
      , cdSkills = [#willpower, #agility, #wild]
      , cdLevel = Just 5
      , cdDeckRestrictions =
          [OnlyInvestigatorWithTraits [Cultist, Cursed, SilverTwilight], PerDeckLimitByTitle 1]
      , cdLimits = [LimitInPlay 3]
      , cdKeywords = setFromList [Keyword.Myriad]
      , cdSlots = [#arcane]
      }

sacredOathOathOfOrder5 :: CardDef
sacredOathOathOfOrder5 =
  fast
    (asset "11122" ("Sacred Oath" <:> "Oath of Order") 1 Neutral)
      { cdCardTraits = setFromList [Pact]
      , cdSkills = [#willpower, #combat, #wild]
      , cdLevel = Just 5
      , cdDeckRestrictions =
          [OnlyInvestigatorWithTraits [Cultist, Cursed, SilverTwilight], PerDeckLimitByTitle 1]
      , cdLimits = [LimitInPlay 3]
      , cdKeywords = setFromList [Keyword.Myriad]
      , cdSlots = [#arcane]
      }

sacredOathOathOfWisdom5 :: CardDef
sacredOathOathOfWisdom5 =
  fast
    (asset "11123" ("Sacred Oath" <:> "Oath of Wisdom") 1 Neutral)
      { cdCardTraits = setFromList [Pact]
      , cdSkills = [#willpower, #intellect, #wild]
      , cdLevel = Just 5
      , cdDeckRestrictions =
          [OnlyInvestigatorWithTraits [Cultist, Cursed, SilverTwilight], PerDeckLimitByTitle 1]
      , cdLimits = [LimitInPlay 3]
      , cdKeywords = setFromList [Keyword.Myriad]
      , cdSlots = [#arcane]
      }

-- The Drowned City — Tasks (permanent story assets)
walkInFaith :: CardDef
walkInFaith =
  permanent
    $ (storyAsset_ "11754a" ("Walk in Faith" <:> "Blind Devotion") Tasks)
      {cdCardTraits = setFromList [Task, Incomplete]}

toeTheLine :: CardDef
toeTheLine =
  permanent
    $ (storyAsset_ "11755a" ("Toe the Line" <:> "Disavowed and Disgraced") Tasks)
      {cdCardTraits = setFromList [Task, Incomplete]}

noPlaceLikeHome :: CardDef
noPlaceLikeHome =
  permanent
    $ (storyAsset_ "11753a" ("No Place Like Home" <:> "Lost and Uncertain") Tasks)
      {cdCardTraits = setFromList [Task, Incomplete]}

goodMoney :: CardDef
goodMoney =
  permanent
    $ (storyAsset_ "11756a" ("Good Money" <:> "The \"Quid\" Part") Tasks)
      {cdCardTraits = setFromList [Task, Incomplete]}

doNoHarm :: CardDef
doNoHarm =
  permanent
    $ (storyAsset_ "11758a" ("Do No Harm" <:> "Can You Save Them?") Tasks)
      {cdCardTraits = setFromList [Task, Incomplete]}

proveYourWorth :: CardDef
proveYourWorth =
  permanent
    $ (storyAsset_ "11757a" ("Prove Your Worth" <:> "Are You a Burden?") Tasks)
      {cdCardTraits = setFromList [Task, Incomplete]}

dreamsOfDestruction :: CardDef
dreamsOfDestruction =
  permanent
    $ (storyAsset_ "11759a" ("Dreams of Destruction" <:> "Visions of a Grim Future") Tasks)
      {cdCardTraits = setFromList [Task, Incomplete]}

plumbTheDepths :: CardDef
plumbTheDepths =
  permanent
    $ (storyAsset_ "11760a" ("Plumb the Depths" <:> "Compulsive Research") Tasks)
      {cdCardTraits = setFromList [Task, Incomplete]}

-- The Drowned City — Expedition story assets
expeditionGear :: CardDef
expeditionGear =
  (storyAsset "11761" ("Expedition Gear" <:> "Tough but Reliable") 2 TdcExpedition)
    { cdCardTraits = setFromList [Item, Trait.Supply]
    , cdSkills = [#combat, #agility, #wild]
    }

laudanum :: CardDef
laudanum =
  (storyAsset "11762" ("Laudanum" <:> "Desperate Measures") 2 TdcExpedition)
    { cdCardTraits = setFromList [Item, Trait.Supply]
    , cdSkills = [#willpower, #willpower]
    }

alienTablet :: CardDef
alienTablet =
  (storyAsset "11763" ("Alien Tablet" <:> "Prehistoric Writings") 3 TdcExpedition)
    { cdCardTraits = setFromList [Item, Relic, Rlyeh]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#hand]
    }

divingSuitTheDrownedCity :: CardDef
divingSuitTheDrownedCity =
  (storyAsset "11764" "Diving Suit" 3 TdcExpedition)
    { cdCardTraits = setFromList [Item, Armor]
    , cdSkills = [#combat]
    , cdSlots = [#body]
    }

rubyStandish :: CardDef
rubyStandish =
  (storyAsset "11765" ("Ruby Standish" <:> "Master Thief") 4 TdcExpedition)
    { cdCardTraits = setFromList [Ally, Criminal]
    , cdSkills = [#agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

andyVanNortwick :: CardDef
andyVanNortwick =
  (storyAsset "11766" ("Andy Van Nortwick" <:> "Ambitious Journalist") 4 TdcExpedition)
    { cdCardTraits = setFromList [Ally, Reporter]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#ally]
    , cdUnique = True
    }

-- The Drowned City — completed Task back sides
noPlaceLikeHomeCompleted :: CardDef
noPlaceLikeHomeCompleted =
  permanent
    $ (storyAsset_ "11753b" ("No Place Like Home" <:> "Where Your Heart Is") Tasks)
      {cdCardTraits = setFromList [Task, Completed]}

walkInFaithCompleted :: CardDef
walkInFaithCompleted =
  permanent
    $ (storyAsset_ "11754b" ("Walk in Faith" <:> "Filled with Spirit") Tasks)
      {cdCardTraits = setFromList [Task, Completed]}

toeTheLineCompleted :: CardDef
toeTheLineCompleted =
  permanent
    $ (storyAsset_ "11755b" ("Toe the Line" <:> "Restored and Resolved") Tasks)
      {cdCardTraits = setFromList [Task, Completed]}

goodMoneyCompleted :: CardDef
goodMoneyCompleted =
  permanent
    $ (storyAsset_ "11756b" ("Good Money" <:> "The \"Quo\" Part") Tasks)
      {cdCardTraits = setFromList [Task, Completed]}

proveYourWorthCompleted :: CardDef
proveYourWorthCompleted =
  permanent
    $ (storyAsset_ "11757b" ("Prove Your Worth" <:> "Standing on Your Own") Tasks)
      {cdCardTraits = setFromList [Task, Completed]}

doNoHarmCompleted :: CardDef
doNoHarmCompleted =
  permanent
    $ (storyAsset_ "11758b" ("Do No Harm" <:> "Reliable Support") Tasks)
      {cdCardTraits = setFromList [Task, Completed]}

dreamsOfDestructionCompleted :: CardDef
dreamsOfDestructionCompleted =
  permanent
    $ (storyAsset_ "11759b" ("Dreams of Destruction" <:> "The Future is Not Fixed") Tasks)
      {cdCardTraits = setFromList [Task, Completed]}

plumbTheDepthsCompleted :: CardDef
plumbTheDepthsCompleted =
  permanent
    $ (storyAsset_ "11760b" ("Plumb the Depths" <:> "Mastered Acumen") Tasks)
      {cdCardTraits = setFromList [Task, Completed]}

-- The Drowned City — encounter story assets (artifacts / relics / allies)
obsidianRelic :: CardDef
obsidianRelic =
  (storyAsset_ "11550" ("Obsidian Relic" <:> "Puzzling Effigy") TheDrownedQuarter)
    { cdCardTraits = setFromList [Item, Relic, Glyph]
    , cdDoubleSided = True
    , cdOtherSide = Just "11550b"
    }

-- Earned Artifact story assets share a distinct card back.
artifactBack :: Map Text Value
artifactBack = mapFromList [("customBack", String "back_artifact.jpg")]

barrierNode :: CardDef
barrierNode =
  (storyAsset_ "11552" ("Barrier Node" <:> "Energy Shield") TheDrownedQuarter)
    { cdCardTraits = setFromList [Item, Artifact, Rlyeh]
    , cdMeta = artifactBack
    , cdUnique = True
    }

mariaRivera :: CardDef
mariaRivera =
  (storyAsset "11568" ("Maria Rivera" <:> "Lost Pilgrim") 3 TheApiary)
    { cdCardTraits = setFromList [Ally, Cultist]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdVictoryPoints = Just 0
    }

ancientRelic :: CardDef
ancientRelic =
  (storyAsset_ "11581" ("Ancient Relic" <:> "Leering Visage") TheApiary)
    { cdCardTraits = setFromList [Item, Relic, Glyph]
    , cdDoubleSided = True
    , cdOtherSide = Just "11581b"
    }

grislyMask :: CardDef
grislyMask =
  (storyAsset_ "11582" ("Grisly \"Mask\"" <:> "Is It Actually Alive?") TheApiary)
    { cdCardTraits = setFromList [Item, Artifact, Rlyeh]
    , cdMeta = artifactBack
    , cdUnique = True
    }

tidalTablet :: CardDef
tidalTablet =
  (storyAsset_ "11611" ("Tidal Tablet" <:> "Record of Ancient Depths") TheGrandVault)
    { cdCardTraits = setFromList [Item, Artifact, Rlyeh]
    , cdMeta = artifactBack
    , cdUnique = True
    }

shardOfYchlecht :: CardDef
shardOfYchlecht =
  (storyAsset_ "11638" ("Shard of Y'ch'lecht" <:> "Otherworldly Remnant") CourtOfTheAncients)
    { cdCardTraits = setFromList [Item, Artifact, Rlyeh]
    , cdMeta = artifactBack
    , cdUnique = True
    }

skyRelic :: CardDef
skyRelic =
  (storyAsset_ "11663" ("Sky Relic" <:> "Eroded by Winds") ObsidianCanyons)
    { cdCardTraits = setFromList [Item, Relic, Glyph]
    , cdDoubleSided = True
    , cdOtherSide = Just "11663b"
    }

obsidianClaw :: CardDef
obsidianClaw =
  (storyAsset_ "11672a" ("Obsidian Claw" <:> "Speed") ObsidianCanyons)
    { cdCardTraits = setFromList [Item, Artifact, Rlyeh]
    , cdMeta = artifactBack
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "11672b"
    }

obsidianClawPower :: CardDef
obsidianClawPower =
  (storyAsset_ "11672b" ("Obsidian Claw" <:> "Power") ObsidianCanyons)
    { cdCardTraits = setFromList [Item, Artifact, Rlyeh]
    , cdMeta = artifactBack
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "11672a"
    }

johnRaymondLegrasse :: CardDef
johnRaymondLegrasse =
  (storyAsset "11687" ("John Raymond Legrasse" <:> "Hunting for Answers") 4 TheDoomOfArkhamPartI)
    { cdCardTraits = setFromList [Ally, Detective]
    , cdSlots = [#ally]
    , cdUnique = True
    }

horrorInClay :: CardDef
horrorInClay =
  (storyAsset_ "11688" ("Horror in Clay" <:> "Sculpture of Dreams") TheDoomOfArkhamPartI)
    { cdCardTraits = setFromList [Item, Artifact, Rlyeh]
    , cdMeta = artifactBack
    , cdUnique = True
    }
