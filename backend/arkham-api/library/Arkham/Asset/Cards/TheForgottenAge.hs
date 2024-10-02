module Arkham.Asset.Cards.TheForgottenAge where

import Arkham.Asset.Cards.Import
import Arkham.ChaosToken.Types qualified as Token
import Arkham.Keyword qualified as Keyword

mitchBrown :: CardDef
mitchBrown =
  signature "04001"
    $ (asset "04006" ("Mitch Brown" <:> "Sole Survivor") 3 Neutral)
      { cdSkills = [#wild, #wild]
      , cdCardTraits = setFromList [Ally, Wayfarer]
      , cdSlots = [#ally]
      , cdUnique = True
      }

jakeWilliams :: CardDef
jakeWilliams =
  signature "04002"
    $ (asset "04008" ("Jake Williams" <:> "Loyal Companion") 3 Neutral)
      { cdSkills = [#intellect, #wild]
      , cdCardTraits = setFromList [Ally, Wayfarer]
      , cdSlots = [#ally]
      , cdUnique = True
      }

finnsTrustyThirtyEight :: CardDef
finnsTrustyThirtyEight =
  fast
    . signature "04003"
    $ (asset "04011" ("Finn's Trusty .38" <:> "Never Leave Home Without It") 2 Neutral)
      { cdSkills = [#agility, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
      , cdSlots = [#hand]
      , cdUses = uses Ammo 3
      , cdUnique = True
      }

theCodexOfAges :: CardDef
theCodexOfAges =
  signature "04004"
    $ (asset "04013" ("The Codex of Ages" <:> "finis omnium nunc est") 2 Neutral)
      { cdSkills = [#willpower, #wild]
      , cdCardTraits = setFromList [Item, Relic, Tome, Blessed]
      , cdSlots = [#hand]
      , cdKeywords = singleton (seal Token.ElderSign)
      , cdUnique = True
      }

untilTheEndOfTime :: CardDef
untilTheEndOfTime =
  signature "04005"
    $ (asset "04015" "Until the End of Time" 1 Neutral)
      { cdSkills = [#combat, #wild]
      , cdCardTraits = singleton Talent
      }

survivalKnife :: CardDef
survivalKnife =
  (asset "04017" "Survival Knife" 2 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

venturer :: CardDef
venturer =
  (asset "04018" "Venturer" 4 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdUses = uses Supply 3
    }

drElliHorowitz :: CardDef
drElliHorowitz =
  (asset "04021" ("Dr. Elli Horowitz" <:> "Assistant Curator") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Assistant]
    , cdSlots = [#ally]
    , cdUnique = True
    }

ancientStone1 :: CardDef
ancientStone1 =
  (asset "04022" ("Ancient Stone" <:> "Unidentified") 1 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#hand]
    , cdLevel = Just 1
    }

toothOfEztli :: CardDef
toothOfEztli =
  (asset "04023" ("Tooth of Eztli" <:> "Mortal Reminder") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    }

treasureHunter1 :: CardDef
treasureHunter1 =
  (asset "04025" "Treasure Hunter" 1 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdLevel = Just 1
    }

decoratedSkull :: CardDef
decoratedSkull =
  (asset "04026" ("Decorated Skull" <:> "Doom Begets Doom") 0 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#accessory]
    , cdUses = uses Charge 0
    }

mistsOfRlyeh :: CardDef
mistsOfRlyeh =
  (asset "04029" "Mists of R'lyeh" 2 Mystic)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    }

theChthonianStone :: CardDef
theChthonianStone =
  (asset "04030" ("The Chthonian Stone" <:> "Stygian Waymark") 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdKeywords =
        singleton
          $ seal
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [Token.Skull, Token.Cultist, Token.Tablet, Token.ElderThing]
    }

protectiveIncantation1 :: CardDef
protectiveIncantation1 =
  (asset "04031" "Protective Incantation" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual, Blessed]
    , cdSlots = [#arcane]
    , cdKeywords = singleton (seal $ ChaosTokenFaceIsNot Token.AutoFail)
    , cdLevel = Just 1
    }

yaotl1 :: CardDef
yaotl1 =
  (asset "04035" ("Yaotl" <:> "Lost Son of Eztli") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 1
    }

backpack :: CardDef
backpack =
  (asset "04037" "Backpack" 2 Neutral)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Item
    , cdSlots = [#body]
    }

alejandroVela :: CardDef
alejandroVela =
  (storyAsset "04051" ("Alejandro Vela" <:> "Renowned Historian") 2 TheUntamedWilds)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdUnique = True
    }

relicOfAgesADeviceOfSomeSort :: CardDef
relicOfAgesADeviceOfSomeSort =
  (storyAsset "04061" ("Relic of Ages" <:> "\8230A Device, of Some Sort") 2 TheDoomOfEztli)
    { cdSkills = [#wild, #wild, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

shrewdAnalysis :: CardDef
shrewdAnalysis =
  permanent
    $ (asset "04106" "Shrewd Analysis" 0 Seeker)
      { cdCardTraits = singleton Talent
      }

luckyCigaretteCase :: CardDef
luckyCigaretteCase =
  (asset "04107" "Lucky Cigarette Case" 2 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["60308"]
    }

fence1 :: CardDef
fence1 =
  (asset "04108" "Fence" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Connection, Illicit]
    , cdLevel = Just 1
    }

arcaneResearch :: CardDef
arcaneResearch =
  permanent
    $ (asset "04109" "Arcane Research" 0 Mystic)
      { cdCardTraits = singleton Talent
      , cdPurchaseTrauma = PurchaseMentalTrauma 1
      }

harlanEarnstone :: CardDef
harlanEarnstone =
  (storyAsset "04118b" ("Harlan Earnstone" <:> "Historical Theorist") 0 ThreadsOfFate)
    { cdCardTraits = setFromList [Bystander, Miskatonic]
    , cdCost = Nothing
    , cdUnique = True
    }

henryDeveau :: CardDef
henryDeveau =
  (storyAsset "04125b" ("Henry Deveau" <:> "Friend of Alejandro") 0 ThreadsOfFate)
    { cdCardTraits = singleton Bystander
    , cdCost = Nothing
    , cdUnique = True
    }

mariaDeSilva :: CardDef
mariaDeSilva =
  (storyAsset "04134b" ("Maria DeSilva" <:> "Wealthy Patron") 0 ThreadsOfFate)
    { cdCardTraits = singleton Bystander
    , cdCost = Nothing
    , cdUnique = True
    }

ichtacaTheForgottenGuardian :: CardDef
ichtacaTheForgottenGuardian =
  (storyAsset "04147" ("Ichtaca" <:> "The Forgotten Guardian") 4 ThreadsOfFate)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Ally, Eztli, Wayfarer]
    , cdUnique = True
    , cdSlots = [#ally]
    }

expeditionJournal :: CardDef
expeditionJournal =
  (storyAsset "04148" "Expedition Journal" 2 ThreadsOfFate)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUnique = True
    }

wellPrepared2 :: CardDef
wellPrepared2 =
  (asset "04151" "Well Prepared" 2 Guardian)
    { cdCardTraits = singleton Talent
    , cdLevel = Just 2
    }

quickStudy2 :: CardDef
quickStudy2 =
  (asset "04154" "Quick Study" 2 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Talent
    , cdLevel = Just 2
    }

highRoller2 :: CardDef
highRoller2 =
  (asset "04156" "High Roller" 2 Rogue)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = singleton Talent
    , cdLevel = Just 2
    }

recallTheFuture2 :: CardDef
recallTheFuture2 =
  (asset "04158" "Recall the Future" 2 Mystic)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Augury, Ritual]
    , cdLevel = Just 2
    }

tryAndTryAgain1 :: CardDef
tryAndTryAgain1 =
  (asset "04159" "Try and Try Again" 2 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    , cdUses = uses Try 3
    , cdLevel = Just 1
    }

cornered2 :: CardDef
cornered2 =
  (asset "04160" "Cornered" 2 Survivor)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Talent
    , cdLevel = Just 2
    }

relicOfAgesForestallingTheFuture :: CardDef
relicOfAgesForestallingTheFuture =
  (storyAsset "04191" ("Relic of Ages" <:> "Forestalling the Future") 2 TheBoundaryBeyond)
    { cdSkills = [#wild, #wild, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

otherworldlyCompass2 :: CardDef
otherworldlyCompass2 =
  (asset "04194" "Otherworldly Compass" 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

lolaSantiago3 :: CardDef
lolaSantiago3 =
  (asset "04196" ("Lola Santiago" <:> "No-Nonsense Archaeologist") 3 Rogue)
    { cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#ally]
    , cdLevel = Just 3
    , cdUnique = True
    }

oliveMcBride :: CardDef
oliveMcBride =
  (asset "04197" ("Olive McBride" <:> "Will Try Anything Once") 2 Mystic)
    { cdCardTraits = setFromList [Ally, Witch]
    , cdSkills = [#willpower]
    , cdSlots = [#ally]
    , cdUnique = True
    }

trenchCoat :: CardDef
trenchCoat =
  (asset "04203" "Trench Coat" 3 Neutral)
    { cdCardTraits = setFromList [Item, Clothing]
    , cdSkills = [#agility]
    , cdSlots = [#body]
    }

ornateBow3 :: CardDef
ornateBow3 =
  (asset "04204" "Ornate Bow" 4 Neutral)
    { cdCardTraits = setFromList [Item, Relic, Weapon, Ranged]
    , cdSkills = [#combat, #agility]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 1
    , cdLevel = Just 3
    }

m1918Bar4 :: CardDef
m1918Bar4 =
  (asset "04229" "M1918 BAR" 5 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#combat, #combat]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 8
    , cdLevel = Just 4
    }

ancientStoneKnowledgeOfTheElders4 :: CardDef
ancientStoneKnowledgeOfTheElders4 =
  (asset "04230" ("Ancient Stone" <:> "Knowledge of the Elders") 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#hand]
    , cdUses = Uses Secret (RecordedCount YouHaveIdentifiedTheStone)
    , cdKeywords = setFromList [Keyword.Researched YouHaveIdentifiedTheStone]
    , cdLevel = Just 4
    }

ancientStoneMindsInHarmony4 :: CardDef
ancientStoneMindsInHarmony4 =
  (asset "04231" ("Ancient Stone" <:> "Minds in Harmony") 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#willpower, #willpower]
    , cdSlots = [#hand]
    , cdUses = Uses Secret (RecordedCount YouHaveIdentifiedTheStone)
    , cdKeywords = setFromList [Keyword.Researched YouHaveIdentifiedTheStone]
    , cdLevel = Just 4
    }

crystallineElderSign3 :: CardDef
crystallineElderSign3 =
  (asset "04235" "Crystalline Elder Sign" 3 Mystic)
    { cdCardTraits = setFromList [Item, Relic, Blessed]
    , cdSkills = [#wild]
    , cdSlots = [#accessory]
    , cdKeywords =
        singleton
          $ seal
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [Token.PlusOne, Token.ElderSign]
    , cdLevel = Just 3
    }

onYourOwn3 :: CardDef
onYourOwn3 =
  (asset "04236" "On Your Own" 2 Survivor)
    { cdCardTraits = singleton Talent
    , cdSkills = [#willpower]
    , cdLimits = [LimitPerInvestigator 1]
    , cdLevel = Just 3
    }

theCustodian :: CardDef
theCustodian =
  (storyAsset "04256" ("The Custodian" <:> "Curious Yithian") 0 TheCityOfArchives)
    { cdCardTraits = setFromList [Ally, Yithian]
    , cdUnique = True
    , cdCost = Nothing
    }

handcuffs :: CardDef
handcuffs =
  (asset "04265" "Handcuffs" 2 Guardian)
    { cdCardTraits = setFromList [Item, Police]
    , cdSkills = [#agility]
    }

feedTheMind3 :: CardDef
feedTheMind3 =
  (asset "04267" "Feed the Mind" 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Spell
    , cdUses = uses Secret 3
    , cdSlots = [#arcane]
    , cdLevel = Just 3
    }

coltVestPocket :: CardDef
coltVestPocket =
  (asset "04268" "Colt Vest Pocket" 2 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 5
    , cdSlots = [#hand]
    }

theSkeletonKey2 :: CardDef
theSkeletonKey2 =
  fast
    $ (asset "04270" "The Skeleton Key" 3 Rogue)
      { cdSkills = [#intellect, #intellect]
      , cdCardTraits = setFromList [Item, Relic, Cursed]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Exceptional]
      , cdLevel = Just 2
      }

mistsOfRlyeh4 :: CardDef
mistsOfRlyeh4 =
  (asset "04271" "Mists of R'lyeh" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 5
    , cdLevel = Just 4
    }

oldHuntingRifle3 :: CardDef
oldHuntingRifle3 =
  (asset "04273" "Old Hunting Rifle" 3 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 3
    , cdLevel = Just 3
    }

thermos :: CardDef
thermos =
  (asset "04274" "Thermos" 4 Neutral)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Item
    , cdUses = uses Supply 3
    }

hemisphericMap3 :: CardDef
hemisphericMap3 =
  (asset "04275" "Hemispheric Map" 2 Neutral)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    , cdLevel = Just 3
    }

timewornBrand5 :: CardDef
timewornBrand5 =
  (asset "04276" "Timeworn Brand" 5 Neutral)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = Just 5
    }

relicOfAgesRepossessThePast :: CardDef
relicOfAgesRepossessThePast =
  (storyAsset "04303" ("Relic of Ages" <:> "Repossess the Past") 2 TheDepthsOfYoth)
    { cdSkills = [#wild, #wild, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

kerosene1 :: CardDef
kerosene1 =
  (asset "04304" "Kerosene" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Item
    , cdUses = uses Supply 3
    , cdLevel = Just 1
    }

flamethrower5 :: CardDef
flamethrower5 =
  (asset "04305" "Flamethrower" 4 Guardian)
    { cdSkills = [#combat, #combat, #wild]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUses = uses Ammo 4
    , cdSlots = [#body, #hand, #hand]
    , cdLevel = Just 5
    }

pnakoticManuscripts5 :: CardDef
pnakoticManuscripts5 =
  (asset "04307" ("Pnakotic Manuscripts" <:> "Mind-Expanding Ideas") 5 Seeker)
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdUses = uses Secret 3
    , cdSlots = [#hand]
    , cdLevel = Just 5
    , cdUnique = True
    }

borrowedTime3 :: CardDef
borrowedTime3 =
  (asset "04308" "Borrowed Time" 1 Rogue)
    { cdSkills = [#willpower, #agility]
    , cdKeywords = singleton Keyword.Exceptional
    , cdCardTraits = singleton Ritual
    , cdSlots = [#arcane]
    , cdLevel = Just 3
    }

shardsOfTheVoid3 :: CardDef
shardsOfTheVoid3 =
  (asset "04310" "Shards of the Void" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdKeywords = singleton $ seal Token.Zero
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = Just 3
    }

sealOfTheSeventhSign5 :: CardDef
sealOfTheSeventhSign5 =
  (asset "04311" ("Seal of the Seventh Sign" <:> "Over the Threshold and Beyond") 4 Mystic)
    { cdSkills = [#willpower, #wild]
    , cdKeywords = singleton $ seal Token.AutoFail
    , cdCardTraits = setFromList [Spell, Ritual]
    , cdUses = uses Charge 7
    , cdSlots = [#arcane]
    , cdLevel = Just 5
    , cdUnique = True
    }

relicOfAgesUnleashTheTimestream :: CardDef
relicOfAgesUnleashTheTimestream =
  (storyAsset "04343" ("Relic of Ages" <:> "Unleash the Timestream") 2 ShatteredAeons)
    { cdSkills = [#wild, #wild, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

-- created via the Intepid Skill
intrepid :: CardDef
intrepid =
  (asset "xintrepid" "Intrepid" 0 Guardian) {cdCardTraits = singleton Innate}
