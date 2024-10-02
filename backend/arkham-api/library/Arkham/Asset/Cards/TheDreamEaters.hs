module Arkham.Asset.Cards.TheDreamEaters where

import Arkham.Asset.Cards.Import
import Arkham.Criteria qualified as Criteria
import Arkham.Keyword qualified as Keyword

becky :: CardDef
becky =
  signature "06001"
    $ (asset "06006" ("Becky" <:> "Custom Marlin Model 1894") 2 Neutral)
      { cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdSkills = [#combat, #agility, #wild]
      , cdSlots = [#hand, #hand]
      , cdUses = uses Ammo 2
      , cdUnique = True
      }

bountyContracts :: CardDef
bountyContracts =
  permanent
    . signature "06003"
    $ (asset "06010" "Bounty Contracts" 0 Neutral)
      { cdCardTraits = setFromList [Job]
      , cdUses = uses Bounty 6
      }

tonys38LongColt :: CardDef
tonys38LongColt =
  signature "06003"
    $ (asset "06011" "Tony's .38 Long Colt" 3 Neutral)
      { cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdSkills = [#combat, #intellect, #wild]
      , cdUses = uses Ammo 3
      , cdSlots = [#hand]
      }

gateBox :: CardDef
gateBox =
  signature "06004"
    $ (asset "06013" ("Gate Box" <:> "Worlds within Worlds") 3 Neutral)
      { cdCardTraits = setFromList [Item, Relic]
      , cdUses = uses Charge 3
      , cdUnique = True
      }

patricesViolin :: CardDef
patricesViolin =
  signature "06005"
    $ (asset "06016" ("Patrice's Violin" <:> "My Muse") 2 Neutral)
      { cdCardTraits = setFromList [Item, Instrument]
      , cdUnique = True
      , cdSlots = [#hand]
      , cdSkills = [#willpower, #agility, #wild]
      }

theHungeringBlade1 :: CardDef
theHungeringBlade1 =
  (asset "06018" ("The Hungering Blade" <:> "Calamitous Blade of Celephaïs") 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee, Relic, Cursed]
    , cdSlots = [#hand]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdAdditionalCost = Just (ShuffleBondedCost 3 "06019")
    , cdLevel = Just 1
    , cdBondedWith = [(3, "06019")]
    , cdUnique = True
    }

solemnVow :: CardDef
solemnVow =
  fast
    $ (asset "06020" "Solemn Vow" 0 Guardian)
      { cdSkills = [#willpower, #willpower]
      , cdCardTraits = singleton Spirit
      , cdKeywords = singleton Keyword.Myriad
      , cdCriteria =
          Just $ Criteria.InvestigatorExists $ affectsOthers $ NotYou <> InvestigatorAt YourLocation
      }

segmentOfOnyx1 :: CardDef
segmentOfOnyx1 =
  fast
    $ (asset "06021" "Segment of Onyx" 1 Seeker)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Item, Relic, Occult]
      , cdLevel = Just 1
      , cdBondedWith = [(1, "06022")]
      , cdKeywords = singleton Keyword.Myriad
      }

pendantOfTheQueen :: CardDef
pendantOfTheQueen =
  (asset "06022" ("Pendant of the Queen" <:> "Of Nothing at All") 0 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdKeywords = singleton (Keyword.Bonded 1 "06021")
    , cdUses = uses Charge 3
    , cdSlots = [#accessory]
    , cdCost = Nothing
    , cdUnique = True
    , cdLevel = Nothing
    }

crystallizerOfDreams :: CardDef
crystallizerOfDreams =
  (asset "06024" "Crystallizer of Dreams" 1 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdBondedWith = [(1, "06025")]
    , cdAdditionalCost = Just (ShuffleBondedCost 1 "06025")
    , cdSlots = [#accessory]
    }

missDoyle1 :: CardDef
missDoyle1 =
  (asset "06030" ("Miss Doyle" <:> "Cat General of Ulthar") 3 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Ally, Creature, Dreamlands]
    , cdLevel = Just 1
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdBondedWith = [(1, "06031"), (1, "06032"), (1, "06033")]
    , cdSlots = [#ally]
    , cdUnique = True
    }

hope :: CardDef
hope =
  fast
    $ (asset "06031" "Hope" 1 Survivor)
      { cdSkills = [#intellect, #combat]
      , cdCardTraits = setFromList [Ally, Creature, Dreamlands]
      , cdKeywords = singleton (Keyword.Bonded 1 "06030")
      , cdUnique = True
      , cdLevel = Nothing
      }

zeal :: CardDef
zeal =
  fast
    $ (asset "06032" "Zeal" 1 Survivor)
      { cdSkills = [#intellect, #agility]
      , cdCardTraits = setFromList [Ally, Creature, Dreamlands]
      , cdKeywords = singleton (Keyword.Bonded 1 "06030")
      , cdUnique = True
      , cdLevel = Nothing
      }

augur :: CardDef
augur =
  fast
    $ (asset "06033" "Augur" 1 Survivor)
      { cdSkills = [#combat, #agility]
      , cdCardTraits = setFromList [Ally, Creature, Dreamlands]
      , cdKeywords = singleton (Keyword.Bonded 1 "06030")
      , cdUnique = True
      , cdLevel = Nothing
      }

kleptomania :: CardDef
kleptomania =
  (basicWeakness "06036" "Kleptomania")
    { cdCardTraits = setFromList [Madness, Talent]
    , cdDeckRestrictions = [MultiplayerOnly]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    }

randolphCarterExpertDreamer :: CardDef
randolphCarterExpertDreamer =
  (storyAsset "06059" ("Randolph Carter" <:> "Expert Dreamer") 3 BeyondTheGatesOfSleep)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Ally, Dreamer]
    , cdSlots = [#ally]
    , cdUnique = True
    }

randolphCarterChainedToTheWakingWorld :: CardDef
randolphCarterChainedToTheWakingWorld =
  (storyAsset "06079" ("Randolph Carter" <:> "Chained to the Waking World") 3 WakingNightmare)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Dreamer]
    , cdSlots = [#ally]
    , cdUnique = True
    }

drShivaniMaheswaran :: CardDef
drShivaniMaheswaran =
  (storyAsset "06080" ("Dr. Shivani Maheswaran" <:> "Emergency Physician") 0 WakingNightmare)
    { cdCardTraits = setFromList [Ally, Medic]
    , cdUnique = True
    , cdCost = Nothing
    }

dreamDiary :: CardDef
dreamDiary =
  (asset "06112" ("Dream Diary" <:> "Untranslated") 2 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tome, Charm]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "06113")]
    }

scrollOfProphecies :: CardDef
scrollOfProphecies =
  (asset "06116" "Scroll of Prophecies" 3 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 4
    , cdSlots = [#hand]
    }

jessicaHyde1 :: CardDef
jessicaHyde1 =
  (asset "06118" ("Jessica Hyde" <:> "Wrong Place, Wrong Time") 3 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Wayfarer, Cursed]
    , cdLevel = Just 1
    , cdSlots = [#ally]
    , cdUnique = True
    }

virgilGray :: CardDef
virgilGray =
  (storyAsset "06144" ("Virgil Gray" <:> "Writer of Strange Tales") 0 TheSearchForKadath)
    { cdCardTraits = setFromList [Ally, Dreamer]
    , cdUnique = True
    , cdCost = Nothing
    }

tetsuoMori :: CardDef
tetsuoMori =
  (asset "06155" ("Tetsuo Mori" <:> "Too Noble for His Own Good") 3 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Police]
    , cdSlots = [#ally]
    , cdUnique = True
    }

otherworldCodex2 :: CardDef
otherworldCodex2 =
  (asset "06158" "Otherworld Codex" 3 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 3
    , cdLevel = Just 2
    }

dreamEnhancingSerum :: CardDef
dreamEnhancingSerum =
  (asset "06159" "Dream-Enhancing Serum" 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Science]
    , cdSlots = [#arcane]
    }

gregoryGry :: CardDef
gregoryGry =
  (asset "06162" ("Gregory Gry" <:> "Muckraker") 3 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Criminal, Dreamer]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Resource 9
    }

healingWords :: CardDef
healingWords =
  (asset "06163" "Healing Words" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    }

versatile2 :: CardDef
versatile2 =
  permanent
    $ (asset "06167" "Versatile" 0 Neutral)
      { cdCardTraits = singleton Talent
      , cdLevel = Just 2
      }

theSilverKey :: CardDef
theSilverKey =
  (storyAsset "06189" ("The Silver Key" <:> "Key to the Gate of Dreams") 2 AThousandShapesOfHorror)
    { cdSkills = [#willpower, #wild, #wild]
    , cdCardTraits = setFromList [Item, Charm, Relic]
    , cdUnique = True
    , cdSlots = [#accessory]
    }

thirtyFiveWinchester :: CardDef
thirtyFiveWinchester =
  (asset "06195" ".35 Winchester" 4 Guardian)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 5
    }

safeguard2 :: CardDef
safeguard2 =
  (asset "06196" "Safeguard" 2 Guardian)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Talent
    , cdLevel = Just 2
    }

burglary2 :: CardDef
burglary2 =
  (asset "06200" "Burglary" 1 Rogue)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Talent, Illicit]
    , cdLevel = Just 2
    }

moonstone :: CardDef
moonstone =
  (asset "06203" "Moonstone" 3 Survivor)
    { cdCardTraits = setFromList [Item, Relic, Dreamlands]
    , cdSlots = [#accessory]
    , cdCriteria = Just Criteria.Never
    , cdCommitRestrictions = [SelfCanCommitWhen NoOne]
    , cdCardInHandEffects = True
    , cdCardInDiscardEffects = True
    }

virgilGrayTrulyInspired :: CardDef
virgilGrayTrulyInspired =
  (storyAsset "06224" ("Virgil Gray" <:> "Truly Inspired") 0 DarkSideOfTheMoon)
    { cdCardTraits = setFromList [Ally, Dreamer]
    , cdUnique = True
    , cdCost = Nothing
    }

theCaptain :: CardDef
theCaptain =
  (storyAsset "06225" ("The Captain" <:> "Dreamlands Navigator") 0 DarkSideOfTheMoon)
    { cdCardTraits = setFromList [Ally, Dreamer]
    , cdUnique = True
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

dreamDiaryDreamsOfAnExplorer3 :: CardDef
dreamDiaryDreamsOfAnExplorer3 =
  (asset "06236" ("Dream Diary" <:> "Dreams of an Explorer") 2 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Tome, Charm]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "06113")]
    , cdLevel = Just 3
    }

dreamDiaryDreamsOfAMadman3 :: CardDef
dreamDiaryDreamsOfAMadman3 =
  (asset "06237" ("Dream Diary" <:> "Dreams of a Madman") 2 Seeker)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Tome, Charm]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "06113")]
    , cdLevel = Just 3
    }

dreamDiaryDreamsOfAChild3 :: CardDef
dreamDiaryDreamsOfAChild3 =
  (asset "06238" ("Dream Diary" <:> "Dreams of a Child") 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome, Charm]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "06113")]
    , cdLevel = Just 3
    }

haste2 :: CardDef
haste2 =
  (asset "06239" "Haste" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Ritual
    , cdSlots = [#arcane]
    , cdLevel = Just 2
    , cdLimits = [LimitPerInvestigator 1]
    }

empowerSelfStamina2 :: CardDef
empowerSelfStamina2 =
  (asset "06241" ("Empower Self" <:> "Stamina") 3 Mystic)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdLevel = Just 2
    , cdKeywords = singleton Keyword.Myriad
    }

empowerSelfAlacrity2 :: CardDef
empowerSelfAlacrity2 =
  (asset "06242" ("Empower Self" <:> "Alacrity") 3 Mystic)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdLevel = Just 2
    , cdKeywords = singleton Keyword.Myriad
    }

empowerSelfAcuity2 :: CardDef
empowerSelfAcuity2 =
  (asset "06243" ("Empower Self" <:> "Acuity") 3 Mystic)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdLevel = Just 2
    , cdKeywords = singleton Keyword.Myriad
    }

twilaKatherinePrice3 :: CardDef
twilaKatherinePrice3 =
  (asset "06244" ("Twila Katherine Price" <:> "Lost in a Dream") 3 Mystic)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Ally, Artist, Dreamer]
    , cdSlots = [#ally]
    , cdLevel = Just 3
    , cdUnique = True
    }

richardUptonPickman :: CardDef
richardUptonPickman =
  (storyAsset "06266" ("Richard Upton Pickman" <:> "Venerable Ghoul") 0 PointOfNoReturn)
    { cdCardTraits = setFromList [Ally, Ghoul, Artist]
    , cdUnique = True
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

emptyVessel4 :: CardDef
emptyVessel4 =
  (asset "06276" ("Empty Vessel" <:> "Abandoned by the Gods") 1 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic, Blessed]
    , cdSlots = [#accessory]
    , cdLevel = Just 4
    , cdUnique = True
    , cdUses = uses Charge 0
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdBondedWith = [(1, "06277")]
    }

wishEater :: CardDef
wishEater =
  (asset "06277" ("Wish Eater" <:> "Jewel of the Gods") 0 Guardian)
    { cdCardTraits = setFromList [Item, Relic, Blessed]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdKeywords = singleton (Keyword.Bonded 1 "06277")
    , cdCost = Nothing
    , cdLevel = Nothing
    }

oldBookOfLore3 :: CardDef
oldBookOfLore3 =
  (asset "06279" "Old Book of Lore" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01686"]
    , cdUses = uses Secret 2
    , cdLevel = Just 3
    }

garroteWire2 :: CardDef
garroteWire2 =
  (asset "06280" "Garrote Wire" 2 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon]
    , cdSlots = [#accessory]
    , cdLevel = Just 2
    }

delilahORourke3 :: CardDef
delilahORourke3 =
  (asset "06281" ("Delilah O'Rourke" <:> "Syndicate Assassin") 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Ally, Criminal, Syndicate]
    , cdSlots = [#ally]
    , cdLevel = Just 3
    , cdUnique = True
    }

summonedHound1 :: CardDef
summonedHound1 =
  (asset "06282" "Summoned Hound" 3 Mystic)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Ally, Summon]
    , cdSlots = [#ally, #arcane]
    , cdAdditionalCost = Just (ShuffleBondedCost 1 "06283")
    , cdLevel = Just 1
    , cdBondedWith = [(1, "06283")]
    }

theBlackCat5 :: CardDef
theBlackCat5 =
  (asset "06285" ("The Black Cat" <:> "A Liar, or a Prophet, or Both") 2 Neutral)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Ally, Avatar, Dreamlands]
    , cdSlots = [#ally]
    , cdLevel = Just 5
    , cdUnique = True
    }

spiritualResolve5 :: CardDef
spiritualResolve5 =
  (asset "06323" "Spiritual Resolve" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual]
    , cdKeywords = singleton Keyword.Myriad
    , cdSlots = [#arcane]
    , cdLevel = Just 5
    }

abigailForeman4 :: CardDef
abigailForeman4 =
  (asset "06324" ("Abigail Foreman" <:> "Library Intern") 3 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 4
    }

joeyTheRatVigil3 :: CardDef
joeyTheRatVigil3 =
  (asset "06326" ("Joey \"The Rat\" Vigil" <:> "Lookin' Out for #1") 2 Rogue)
    { cdCardTraits = setFromList [Ally, Criminal]
    , cdSkills = [#intellect, #agility]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdLevel = Just 3
    }

sawedOffShotgun5 :: CardDef
sawedOffShotgun5 =
  (asset "06327" "Sawed-Off Shotgun" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdLevel = Just 5
    , cdUses = uses Ammo 2
    , cdSlots = [#hand]
    }

mindsEye2 :: CardDef
mindsEye2 =
  (asset "06328" "Mind's Eye" 3 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual]
    , cdUses = uses Secret 3
    , cdSlots = [#arcane, #arcane]
    , cdLevel = Just 2
    }

shiningTrapezohedron4 :: CardDef
shiningTrapezohedron4 =
  (asset "06329" "Shining Trapezohedron" 1 Mystic)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    , cdLevel = Just 4
    , cdUnique = True
    }

nightmareBauble3 :: CardDef
nightmareBauble3 =
  (asset "06330" "Nightmare Bauble" 1 Survivor)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdSlots = [#accessory]
    , cdLevel = Just 3
    , cdBondedWith = [(3, "06331")]
    }

scavenging2 :: CardDef
scavenging2 =
  (asset "06332" "Scavenging" 1 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 2
    }
