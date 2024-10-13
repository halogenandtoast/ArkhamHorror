module Arkham.Asset.Cards.TheScarletKeys where

import Arkham.Asset.Cards.Import
import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.Customization
import Arkham.Keyword qualified as Keyword

bonesaw :: CardDef
bonesaw =
  signature "09004"
    $ (asset "09005" "Bonesaw" 3 Neutral)
      { cdCardTraits = setFromList [Item, Tool, Melee]
      , cdSkills = [#intellect, #combat, #wild]
      , cdSlots = [#hand]
      }

woundedBystanderOnDeathsDoorstep :: CardDef
woundedBystanderOnDeathsDoorstep =
  (weakness "09007" ("Wounded Bystander" <:> "On Death's Doorstep"))
    { cdCardTraits = setFromList [Ally, Bystander]
    }

grapplingHook :: CardDef
grapplingHook =
  signature "09008"
    $ (asset "09009" "Grappling Hook" 3 Neutral)
      { cdCardTraits = setFromList [Item, Tool]
      , cdSkills = [#intellect, #agility, #wild]
      , cdSlots = [#hand]
      }

darrellsKodak :: CardDef
darrellsKodak =
  signature "09015"
    $ (asset "09016" ("Darrell's Kodak" <:> "Proof in the Pudding") 2 Neutral)
      { cdCardTraits = setFromList [Item, Tool]
      , cdSkills = [#intellect, #agility, #wild]
      }

bonnieWalshLoyalAssistant :: CardDef
bonnieWalshLoyalAssistant =
  signature "09018"
    $ (asset "09019" ("Bonnie Walsh" <:> "Loyal Assistant") 3 Neutral)
      { cdCardTraits = setFromList [Ally, Civic, Assistant]
      , cdSkills = [#wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

huntersArmor :: CardDef
huntersArmor =
  (asset "09021" "Hunter's Armor" 4 Guardian)
    { cdCardTraits = setFromList [Item, Armor]
    , cdSkills = [#willpower]
    , cdSlots = [#body]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdCardInHandEffects = True
    , cdCustomizations =
        mapFromList
          [ (Enchanted, 1)
          , (ProtectiveRunes, 2)
          , (Durable, 2)
          , (Hallowed, 2)
          , (Lightweight, 2)
          , (Hexdrinker, 3)
          , (ArmorOfThorns, 3)
          ]
    }

runicAxe :: CardDef
runicAxe =
  (asset "09022" "Runic Axe" 4 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Charge 4
    , cdUnique = True
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdCardInHandEffects = True
    , cdCustomizations =
        mapFromList
          [ (Heirloom, 1)
          , (InscriptionOfGlory, 1)
          , (InscriptionOfTheElders, 1)
          , (InscriptionOfTheHunt, 1)
          , (InscriptionOfFury, 1)
          , (AncientPower, 3)
          , (Saga, 3)
          , (Scriptweaver, 4)
          ]
    }

obsidianBracelet :: CardDef
obsidianBracelet =
  (asset "09024" "Obsidian Bracelet" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#hand]
    }

bestowResolve2 :: CardDef
bestowResolve2 =
  (asset "09032" "Bestow Resolve" 2 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    , cdLevel = Just 2
    }

fieldAgent2 :: CardDef
fieldAgent2 =
  (asset "09033" "Field Agent" 4 Guardian)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Ally, Agency]
    , cdSlots = [#ally]
    , cdLevel = Just 2
    }

guardDog2 :: CardDef
guardDog2 =
  (asset "09034" "Guard Dog" 3 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Ally, Creature]
    , cdSlots = [#ally]
    , cdLevel = Just 2
    }

handcuffs2 :: CardDef
handcuffs2 =
  fast
    $ (asset "09035" "Handcuffs" 1 Guardian)
      { cdCardTraits = setFromList [Item, Police]
      , cdSkills = [#combat, #agility]
      , cdLevel = Just 2
      }

martyrsVambraceRemnantOfTheUnknown3 :: CardDef
martyrsVambraceRemnantOfTheUnknown3 =
  (asset "09037" ("Martyr's Vambrace" <:> "Remnant of the Unknown") 2 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Charm, Armor]
    , cdSlots = [#accessory]
    , cdLevel = Just 3
    , cdUnique = True
    }

girishKadakiaIcpcPunjabDetective4 :: CardDef
girishKadakiaIcpcPunjabDetective4 =
  (asset "09038" ("Girish Kadakia" <:> "ICPC Punjab Detective") 4 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Ally, Police]
    , cdSlots = [#ally]
    , cdLevel = Just 4
    , cdUnique = True
    }

alchemicalDistillation :: CardDef
alchemicalDistillation =
  (asset "09040" "Alchemical Distillation" 2 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#willpower]
    , cdSlots = [#hand]
    , cdUses = uses Supply 3
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdCustomizations =
        mapFromList
          [ (MendingDistillate, 1)
          , (CalmingDistillate, 1)
          , (EnlighteningDistillate, 1)
          , (QuickeningDistillate, 1)
          , (Refined, 2)
          , (Empowered, 4)
          , (Perfected, 5)
          ]
    }

empiricalHypothesis :: CardDef
empiricalHypothesis =
  (asset "09041" "Empirical Hypothesis" 2 Seeker)
    { cdCardTraits = setFromList [Talent, Science]
    , cdSkills = [#intellect]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdLimits = [LimitPerInvestigator 1]
    , cdUses = uses Evidence 0
    , cdCustomizations =
        mapFromList
          [ (PessimisticOutlook, 1)
          , (TrialAndError, 1)
          , (IndepedentVariable, 1)
          , (FieldResearch, 1)
          , (PeerReview, 2)
          , (ResearchGrant, 2)
          , (IrrefutableProof, 3)
          , (AlternativeHypothesis, 4)
          ]
    }

dissectionTools :: CardDef
dissectionTools =
  (asset "09043" "Dissection Tools" 2 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Tool, Science]
    , cdSlots = [#hand]
    }

grimMemoir :: CardDef
grimMemoir =
  (asset "09044" "Grim Memoir" 3 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    }

researchNotes :: CardDef
researchNotes =
  (asset "09045" "Research Notes" 1 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome, Science]
    , cdSlots = [#hand]
    }

labCoat1 :: CardDef
labCoat1 =
  (asset "09050" "Lab Coat" 2 Seeker)
    { cdCardTraits = setFromList [Item, Clothing, Science]
    , cdSkills = [#willpower]
    , cdSlots = [#body]
    , cdLevel = Just 1
    }

orphicTheory1 :: CardDef
orphicTheory1 =
  (asset "09051" "Orphic Theory" 2 Seeker)
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#intellect]
    , cdSlots = [#arcane]
    , cdLevel = Just 1
    , cdUses = uses Secret 4
    }

drWilliamTMaleson2 :: CardDef
drWilliamTMaleson2 =
  (asset "09054" ("Dr. William T. Maleson" <:> "Working on Something Bigger") 1 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdLevel = Just 2
    }

pressPass2 :: CardDef
pressPass2 =
  (asset "09055" "Press Pass" 4 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdLevel = Just 2
    }

surgicalKit3 :: CardDef
surgicalKit3 =
  (asset "09056" "Surgical Kit" 2 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tool, Science]
    , cdUses = uses Supply 4
    , cdLevel = Just 2
    }

fingerprintKit4 :: CardDef
fingerprintKit4 =
  (asset "09057" "Fingerprint Kit" 5 Seeker)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#hand]
    , cdUses = uses Supply 3
    , cdLevel = Just 4
    }

graysAnatomyTheDoctorsBible5 :: CardDef
graysAnatomyTheDoctorsBible5 =
  (asset "09058" ("Gray's Anatomy" <:> "The Doctors' Bible") 3 Seeker)
    { cdCardTraits = setFromList [Item, Tome]
    , cdSkills = [#willpower, #intellect, #wild]
    , cdSlots = [#hand]
    , cdLevel = Just 5
    }

damningTestimony :: CardDef
damningTestimony =
  (asset "09059" "Damning Testimony" 4 Rogue)
    { cdCardTraits = setFromList [Item, Illicit]
    , cdSkills = [#intellect]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdUses = uses Evidence 3
    , cdSlots = [#hand]
    , cdCustomizations =
        mapFromList
          [ (SearchWarrant, 1)
          , (FabricatedEvidence, 2)
          , (Blackmail, 2)
          , (Extort, 3)
          , (Surveil, 3)
          , (Expose, 4)
          ]
    }

disguise :: CardDef
disguise =
  (asset "09062" "Disguise" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Talent, Trick, Illicit]
    , cdUses = uses Supply 4
    }

embezzledTreasure :: CardDef
embezzledTreasure =
  (asset "09063" "Embezzled Treasure" 0 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Illicit]
    }

thievesKit :: CardDef
thievesKit =
  (asset "09064" "Thieves' Kit" 3 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool, Illicit]
    , cdUses = uses Supply 6
    , cdSlots = [#hand]
    }

stylishCoat1 :: CardDef
stylishCoat1 =
  (asset "09071" "Stylish Coat" 2 Rogue)
    { cdCardTraits = setFromList [Item, Clothing, Science]
    , cdSkills = [#agility]
    , cdSlots = [#body]
    , cdLevel = Just 1
    }

chuckFergus2 :: CardDef
chuckFergus2 =
  (asset "09072" ("Chuck Fergus" <:> "O'Bannion Driver") 4 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdSlots = [#ally]
    , cdLevel = Just 2
    , cdUnique = True
    }

dirtyFighting2 :: CardDef
dirtyFighting2 =
  (asset "09073" "Dirty Fighting" 3 Rogue)
    { cdCardTraits = setFromList [Talent, Trick, Illicit]
    , cdSkills = [#combat]
    , cdLevel = Just 2
    , cdLimits = [LimitPerInvestigator 1]
    }

thievesKit3 :: CardDef
thievesKit3 =
  (asset "09075" "Thieves' Kit" 3 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Item, Tool, Illicit]
    , cdUses = uses Supply 6
    , cdSlots = [#hand]
    , cdLevel = Just 3
    }

triggerMan3 :: CardDef
triggerMan3 =
  (asset "09076" "Trigger Man" 4 Rogue)
    { cdCardTraits = setFromList [Ally, Criminal]
    , cdSkills = [#combat]
    , cdLevel = Just 3
    , cdSlots = [#ally]
    }

underworldMarket2 :: CardDef
underworldMarket2 =
  permanent
    $ (asset "09077" "Underworld Market" 0 Rogue)
      { cdCardTraits = setFromList [Connection, Illicit]
      , cdExceptional = True
      , cdLevel = Just 2
      }

livingInk :: CardDef
livingInk =
  (asset "09079" "Living Ink" 0 Mystic)
    { cdCardTraits = setFromList [Ritual]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdUses = uses Charge 3
    , cdSlots = [#body]
    , cdCustomizations =
        mapFromList
          [ (ChoicePlaceholder, 0)
          , (ShiftingInk, 1)
          , (SubtleDepiction, 1)
          , (ImbuedInk, 2)
          , (EldritchInk, 2)
          , (EldritchInk2, 3)
          , (MacabreDepiction, 3)
          , (Vibrancy, 3)
          ]
    }

summonedServitor :: CardDef
summonedServitor =
  (asset "09080" "Summoned Servitor" 2 Mystic)
    { cdCardTraits = setFromList [Ritual]
    , cdSkills = [#willpower]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdSlots = [#ally, #arcane]
    , cdUnique = True
    , cdCardInHandEffects = True
    , cdAdditionalCost =
        Just
          $ CostIfCustomization
            DreamingCall
            ( OrCost
                [ReturnMatchingAssetToHandCost $ AssetControlledBy You, DiscardAssetCost $ AssetControlledBy You]
            )
            (DiscardAssetCost $ AssetControlledBy You)
    , cdCustomizations =
        mapFromList
          [ (ArmoredCarapace, 1)
          , (ClawsThatCatch, 1)
          , (JawsThatSnatch, 1)
          , (EyesOfFlame, 1)
          , (WingsOfNight, 1)
          , (Dominance, 2)
          , (DreamingCall, 3)
          , (DæmonicInfluence, 5)
          ]
    }

ceremonialSickle :: CardDef
ceremonialSickle =
  (asset "09082" "Ceremonial Sickle" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Charm, Weapon, Melee]
    , cdSlots = [#hand]
    }

dowsingRod :: CardDef
dowsingRod =
  (asset "09083" "Dowsing Rod" 4 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#hand]
    }

hallowedChalice :: CardDef
hallowedChalice =
  (asset "09084" "Hallowed Chalice" 3 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#hand]
    }

onyxPentacle :: CardDef
onyxPentacle =
  (asset "09085" "Onyx Pentacle" 3 Mystic)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#hand]
    }

bindersJarInterdimensionalPrison1 :: CardDef
bindersJarInterdimensionalPrison1 =
  (asset "09089" ("Binders Jar" <:> "Interdimensional Prison") 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdLevel = Just 1
    }

astralMirror2 :: CardDef
astralMirror2 =
  (asset "09091" "Astral Mirror" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#body]
    , cdLevel = Just 2
    }

elleRubashPurifyingPurpose2 :: CardDef
elleRubashPurifyingPurpose2 =
  (asset "09092" ("Elle Rubash" <:> "Purifying Purpose") 3 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Ally, Witch]
    , cdSlots = [#ally]
    , cdLevel = Just 2
    , cdUnique = True
    }

sinEater3 :: CardDef
sinEater3 =
  permanent
    $ (asset "09094" "Sin-Eater" 0 Mystic)
      { cdCardTraits = setFromList [Ritual]
      , cdLevel = Just 3
      , cdExceptional = True
      }

ceremonialSickle4 :: CardDef
ceremonialSickle4 =
  (asset "09096" "Ceremonial Sickle" 3 Mystic)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Charm, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = Just 4
    }

dowsingRod4 :: CardDef
dowsingRod4 =
  (asset "09097" "Dowsing Rod" 4 Mystic)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#hand]
    , cdLevel = Just 4
    }

onyxPentacle4 :: CardDef
onyxPentacle4 =
  (asset "09098" "Onyx Pentacle" 3 Mystic)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#hand]
    , cdLevel = Just 4
    }

pocketMultiTool :: CardDef
pocketMultiTool =
  (asset "09099" "Pocket Multi Tool" 3 Survivor)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#wild]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdSlots = [#hand]
    , cdLimits = [LimitPerInvestigator 1]
    , cdCustomizations =
        mapFromList
          [ (Detachable, 1)
          , (PryBar, 1)
          , (SharpenedKnife, 2)
          , (SignalMirror, 2)
          , (MagnifyingLens, 2)
          , (LuckyCharm, 3)
          , (SpringLoaded, 4)
          ]
    }

idolOfXanatosWatcherBeyondTime :: CardDef
idolOfXanatosWatcherBeyondTime =
  (asset "09102" ("Idol of Xanatos" <:> "Watcher Beyond Time") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    }

improvisedShield :: CardDef
improvisedShield =
  (asset "09103" "Improvised Shield" 1 Survivor)
    { cdCardTraits = setFromList [Item, Armor, Improvised]
    , cdSlots = [#hand]
    , cdCriteria = Just Criteria.InYourDiscard
    , cdPlayableFromDiscard = True
    }

baseballBat2 :: CardDef
baseballBat2 =
  (asset "09113" "Baseball Bat" 2 Survivor)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand, #hand]
    }

katjaEastbankKeeperOfEsotericLore2 :: CardDef
katjaEastbankKeeperOfEsotericLore2 =
  (asset "09114" ("Katja Eastbank" <:> "Keeper of Esoteric Lore") 3 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Ally, Scholar]
    , cdSlots = [#ally]
    , cdLevel = Just 2
    , cdUnique = True
    }

oldKeyring3 :: CardDef
oldKeyring3 =
  (asset "09117" "Old Keyring" 1 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdUses = uses Uses.Key 3
    , cdSlots = [#hand]
    , cdLevel = Just 3
    }

hyperphysicalShotcasterTheoreticalDevice :: CardDef
hyperphysicalShotcasterTheoreticalDevice =
  (asset "09119" ("Hyperphysical Shotcaster" <:> "Theoretical Device") 4 Neutral)
    { cdCardTraits = setFromList [Item, Relic, Weapon, Firearm]
    , cdSkills = [#wild]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdUses = uses Aether 4
    , cdUnique = True
    , cdSlots = [#hand]
    , cdCustomizations =
        mapFromList
          [ (Railshooter, 2)
          , (Telescanner, 2)
          , (Translocator, 2)
          , (Realitycollapser, 2)
          , (Matterweaver, 2)
          , (AethericLink, 4)
          , (EmpoweredConfiguration, 4)
          ]
    }

toolBelt :: CardDef
toolBelt =
  (asset "09120" "Tool Belt" 2 Neutral)
    { cdCardTraits = setFromList [Item, Clothing]
    , cdSlots = [#body]
    , cdSkills = [#willpower]
    }

flashlight3 :: CardDef
flashlight3 =
  (asset "09122" "Flashlight" 2 Neutral)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Item, Tool]
    , cdUses = uses Supply 4
    , cdSlots = [#hand]
    , cdLevel = Just 3
    }

soulSanctification3 :: CardDef
soulSanctification3 =
  permanent
    (asset "09123" "Soul Sanctification" 0 Neutral)
      { cdCardTraits = setFromList [Ritual]
      , cdLevel = Just 3
      }
