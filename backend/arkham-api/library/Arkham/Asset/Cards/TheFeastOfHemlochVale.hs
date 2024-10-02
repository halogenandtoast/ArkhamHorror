module Arkham.Asset.Cards.TheFeastOfHemlochVale where

import Arkham.Asset.Cards.Import
import Arkham.Criteria qualified as Criteria
import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait

fluxStabilizerInactive :: CardDef
fluxStabilizerInactive =
  signature "10004"
    $ permanent
    $ (asset "10005" ("Flux Stablizer" <:> "Inactive") 0 Guardian)
      { cdCardTraits = setFromList [Item, Tool, Science]
      , cdUnique = True
      , cdBondedWith = [(1, "10006"), (1, "10007")]
      , cdOtherSide = Just "10005b"
      }

fluxStabilizerActive :: CardDef
fluxStabilizerActive =
  signature "10004"
    $ permanent
    $ (asset "10005b" ("Flux Stablizer" <:> "Active") 0 Guardian)
      { cdCardTraits = setFromList [Item, Tool, Science]
      , cdUnique = True
      , cdBondedWith = [(1, "10006"), (1, "10007")]
      , cdOtherSide = Just "10005"
      }

bookOfLivingMythsChronicleOfWonders :: CardDef
bookOfLivingMythsChronicleOfWonders =
  signature "10012"
    $ (asset "10013" ("Book of Living Myths" <:> "Chronicle of Wonders") 2 Neutral)
      { cdSkills = [#willpower, #intellect, #wild]
      , cdCardTraits = setFromList [Item, Tome, Blessed, Cursed]
      , cdUnique = True
      , cdSlots = [#hand]
      }

ancestralToken :: CardDef
ancestralToken =
  (asset "10019" "Ancestral Token" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdSlots = [#accessory]
    }

cleaningKit :: CardDef
cleaningKit =
  (asset "10020" "Cleaning Kit" 3 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#accessory]
    , cdUses = uses Supply 3
    }

katana :: CardDef
katana =
  (asset "10021" "Katana" 4 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand, #hand]
    }

ofuda :: CardDef
ofuda =
  (asset "10022" "Ofuda" 2 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdUses = uses Charge 1
    }

wolfMaskTheMoonsSire :: CardDef
wolfMaskTheMoonsSire =
  (asset "10023" ("Wolf Mask" <:> "The Moon's Sire") 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Charm, Mask]
    , cdUses = uses Offering 2
    , cdLimits = [LimitPerTrait Mask 1]
    }

cleaningKit3 :: CardDef
cleaningKit3 =
  (asset "10033" "Cleaning Kit" 3 Guardian)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#accessory]
    , cdUses = uses Supply 4
    , cdLevel = Just 3
    }

blessedBlade4 :: CardDef
blessedBlade4 =
  (asset "10034" "Blessed Blade" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee, Blessed]
    , cdSkills = [#willpower, #combat]
    , cdSlots = [#hand]
    , cdLevel = Just 4
    }

eyesOfValusiaTheMothersCunning4 :: CardDef
eyesOfValusiaTheMothersCunning4 =
  (asset "10035" ("Eyes of Valusia" <:> "The Mother's Cunning") 3 Guardian)
    { cdCardTraits = setFromList [Item, Relic, Spell]
    , cdSkills = [#willpower, #combat]
    , cdSlots = [#hand, #arcane]
    , cdLevel = Just 4
    , cdUnique = True
    , cdBondedWith = [(1, "10036")]
    }

bladeOfYothTheFathersIre :: CardDef
bladeOfYothTheFathersIre =
  (asset "10036" ("Blade of Yoth" <:> "The Father's Ire") 0 Guardian)
    { cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdSkills = [#combat, #agility]
    , cdSlots = [#hand, #arcane]
    , cdKeywords = singleton (Keyword.Bonded 1 "10035")
    , cdUnique = True
    , cdCost = Nothing
    }

evanescentAscensionTheMorningStar :: CardDef
evanescentAscensionTheMorningStar =
  (asset "10039" ("Evanescent Ascension" <:> "The Morning Star") 0 Guardian)
    { cdCardTraits = setFromList [Ritual, Pact, Blessed]
    , cdSlots = [#arcane]
    , cdKeywords = singleton (Keyword.Bonded 1 "10038")
    , cdUnique = True
    , cdCost = Nothing
    }

chemistrySet :: CardDef
chemistrySet =
  (asset "10040" "Chemistry Set" 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool, Science]
    , cdSlots = [#accessory]
    }

drCharlesWestIiiKnowsHisPurpose :: CardDef
drCharlesWestIiiKnowsHisPurpose =
  (asset "10041" ("Dr. Charles West III" <:> "Knows His Purpose") 3 Seeker)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Ally, Science]
    , cdSlots = [#ally]
    , cdUnique = True
    }

microscope :: CardDef
microscope =
  (asset "10042" "Microscope" 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool, Science]
    , cdSlots = [#hand]
    }

mouseMaskTheMeekWatcher :: CardDef
mouseMaskTheMeekWatcher =
  (asset "10043" ("Mouse Mask" <:> "The Meek Watcher") 1 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Charm, Mask]
    , cdUses = uses Offering 2
    , cdLimits = [LimitPerTrait Mask 1]
    }

ravenousMyconidUnidentified :: CardDef
ravenousMyconidUnidentified =
  (asset "10044" ("Ravenous Myconid" <:> "Unidentified") 2 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Creature, Monster, Flora, Science]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdBondedWith = [(1, "10045")]
    }

gabrielCarilloTrustedConfidante1 :: CardDef
gabrielCarilloTrustedConfidante1 =
  (asset "10052" ("Gabriel Carillo" <:> "Trusted Confidante") 4 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Scholar, Cursed]
    , cdSlots = [#ally]
    , cdLevel = Just 1
    }

steadyHanded1 :: CardDef
steadyHanded1 =
  (asset "10053" "Steady Handed" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Talent, Science]
    , cdLevel = Just 1
    , cdLimits = [LimitPerInvestigator 1]
    }

prismaticSpectaclesLensToTheOtherworld2 :: CardDef
prismaticSpectaclesLensToTheOtherworld2 =
  (asset "10056" ("Prismatic Spectacles" <:> "Lens to the Otherworld") 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#accessory]
    , cdLevel = Just 2
    }

microscope4 :: CardDef
microscope4 =
  (asset "10058" "Microscope" 2 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tool, Science]
    , cdSlots = [#hand]
    , cdLevel = Just 4
    }

ravenousMyconidSentientStrain4 :: CardDef
ravenousMyconidSentientStrain4 =
  (asset "10059" ("Ravenous Myconid" <:> "Sentient Strait") 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Creature, Science]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdKeywords = singleton $ Keyword.Researched YouHaveClassifiedANewSpecies
    , cdBondedWith = [(1, "10045")]
    , cdLevel = Just 4
    }

ravenousMyconidCarnivorousStrain4 :: CardDef
ravenousMyconidCarnivorousStrain4 =
  (asset "10060" ("Ravenous Myconid" <:> "Carnivorous Strain") 2 Seeker)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Monster, Science]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdKeywords = singleton $ Keyword.Researched YouHaveClassifiedANewSpecies
    , cdBondedWith = [(1, "10045")]
    , cdLevel = Just 4
    }

ravenousMyconidNurturingStrain4 :: CardDef
ravenousMyconidNurturingStrain4 =
  (asset "10061" ("Ravenous Myconid" <:> "Nurturing Strain") 2 Seeker)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Flora, Science]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdKeywords = singleton $ Keyword.Researched YouHaveClassifiedANewSpecies
    , cdBondedWith = [(1, "10045")]
    , cdLevel = Just 4
    }

biancaDieKatzSingingYourSong :: CardDef
biancaDieKatzSingingYourSong =
  (asset "10062" ("Bianca \"Die Katz\"" <:> "Singing Your Song") 2 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Ally, Criminal, Socialite]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdUses = uses Resource 10
    , cdSlots = [#ally]
    , cdBondedWith = [(1, "10063")]
    }

blackmailFile :: CardDef
blackmailFile =
  (asset "10064" "Blackmail File" 2 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tome, Illicit]
    }

britishBullDog :: CardDef
britishBullDog =
  (asset "10065" "British Bull Dog" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 3
    , cdCardInHandEffects = True
    }

fakeCredentials :: CardDef
fakeCredentials =
  (asset "10066" "Fake Credentials" 3 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdSlots = [#hand]
    }

foxMaskTheWiseTrickster :: CardDef
foxMaskTheWiseTrickster =
  (asset "10067" ("Fox Mask" <:> "The Wise Trickster") 1 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Item, Charm, Mask]
    , cdUses = uses Offering 2
    , cdLimits = [LimitPerTrait Mask 1]
    }

scrimshawCharmFromDistantShores :: CardDef
scrimshawCharmFromDistantShores =
  (asset "10068" ("Scrimshaw Charm" <:> "From Distant Shores") 1 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdSlots = [#accessory]
    }

britishBullDog2 :: CardDef
britishBullDog2 =
  (asset "10077" "British Bull Dog" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 3
    , cdCardInHandEffects = True
    , cdLevel = Just 2
    }

bewitching3 :: CardDef
bewitching3 =
  permanent
    (asset "10079" "Bewitching" 0 Rogue)
      { cdCardTraits = setFromList [Talent, Trick]
      , cdExceptional = True
      , cdLevel = Just 3
      }

fakeCredentials4 :: CardDef
fakeCredentials4 =
  (asset "10082" "Fake Credentials" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdSlots = [#hand]
    , cdLevel = Just 4
    }

catMaskTheCapriciousMeddler :: CardDef
catMaskTheCapriciousMeddler =
  (asset "10084" ("Cat Mask" <:> "The Capricious Meddler") 1 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Charm, Mask]
    , cdUses = uses Offering 2
    , cdLimits = [LimitPerTrait Mask 1]
    }

rodOfCarnamagosScepterOfTheMadSeer :: CardDef
rodOfCarnamagosScepterOfTheMadSeer =
  (asset "10085" ("Rod of Carnamagos" <:> "Scepter of the Mad Seer") 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic, Occult, Cursed]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "10086"), (1, "10087"), (1, "10088"), (1, "10089"), (1, "10090")]
    }

speakToTheDead :: CardDef
speakToTheDead =
  (asset "10091" "Speak to the Dead" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Talent, Ritual]
    , cdSlots = [#arcane]
    , cdUses = uses Offering 6
    }

wickedAthame :: CardDef
wickedAthame =
  (asset "10092" ("Wicked Athame" <:> "Cursed Blade") 2 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee, Cursed]
    , cdSlots = [#hand]
    }

oliveMcBride2 :: CardDef
oliveMcBride2 =
  (asset "10097" ("Olive McBride" <:> "Will Try Anything Once") 2 Mystic)
    { cdCardTraits = setFromList [Ally, Witch]
    , cdSkills = [#willpower, #willpower]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 2
    }

rodOfCarnamagosScepterOfTheMadSeer2 :: CardDef
rodOfCarnamagosScepterOfTheMadSeer2 =
  (asset "10098" ("Rod of Carnamagos" <:> "Scepter of the Mad Seer") 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic, Occult, Cursed]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "10086"), (1, "10087"), (1, "10088"), (1, "10089"), (1, "10090")]
    }

theKeyOfSolomonSecretsOfTheUnknown4 :: CardDef
theKeyOfSolomonSecretsOfTheUnknown4 =
  (asset "10104" ("The Key of Solomon" <:> "Secrets of the Unknown") 2 Mystic)
    { cdSkills = [#willpower, #intellect, #wild, #wild]
    , cdCardTraits = setFromList [Item, Tome, Blessed, Cursed]
    , cdSlots = [#hand]
    , cdLevel = Just 4
    }

keeperOfTheKeyCelestialWard :: CardDef
keeperOfTheKeyCelestialWard =
  (asset "10106" ("Keeper of the Key" <:> "Celestial Ward") 0 Mystic)
    { cdCardTraits = setFromList [Summon]
    , cdKeywords = singleton (Keyword.Bonded 1 "10105")
    , cdCost = Nothing
    }

servantOfBrassDaemonaicVassal :: CardDef
servantOfBrassDaemonaicVassal =
  (asset "10107" ("Servant of Brass" <:> "Daemonaic Vassal") 0 Mystic)
    { cdCardTraits = setFromList [Summon]
    , cdKeywords = singleton (Keyword.Bonded 1 "10105")
    , cdCost = Nothing
    }

matchbox :: CardDef
matchbox =
  (asset "10108" "Matchbox" 1 Survivor)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect]
    , cdUses = uses Supply 3
    }

peltShipment :: CardDef
peltShipment =
  (asset "10109" "Pelt Shipment" 0 Survivor)
    { cdCardTraits = setFromList [Item, Trait.Supply]
    , cdCost = Nothing
    , cdCriteria = Just Criteria.Never
    , cdCardInHandEffects = True
    }

pitchfork :: CardDef
pitchfork =
  (asset "10110" "Pitchfork" 3 Survivor)
    { cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdSkills = [#combat, #agility]
    , cdSlots = [#hand, #hand]
    }

sparrowMaskTheWanderersCompanion :: CardDef
sparrowMaskTheWanderersCompanion =
  (asset "10111" ("Sparrow Mask" <:> "The Wanderer's Companion") 1 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Charm, Mask]
    , cdUses = uses Offering 2
    , cdLimits = [LimitPerTrait Mask 1]
    }

hatchet1 :: CardDef
hatchet1 =
  (asset "10117" "Hatchet" 2 Survivor)
    { cdCardTraits = setFromList [Item, Tool, Weapon, Ranged]
    , cdSkills = [#agility]
    , cdSlots = [#hand]
    , cdLevel = Just 1
    }

devilFriendOrFoe2 :: CardDef
devilFriendOrFoe2 =
  (asset "10119" ("\"Devil\"" <:> "Friend or Foe?") 1 Survivor)
    { cdCardTraits = setFromList [Ally, Creature, Cursed]
    , cdSkills = [#agility]
    , cdSlots = [#ally]
    , cdLevel = Just 2
    , cdUnique = True
    }

fireAxe2 :: CardDef
fireAxe2 =
  fast
    $ (asset "10120" "Fire Axe" 1 Survivor)
      { cdSkills = [#combat, #combat]
      , cdCardTraits = setFromList [Item, Weapon, Melee]
      , cdSlots = [#hand]
      }

huntingJacket2 :: CardDef
huntingJacket2 =
  (asset "10121" "Hunting Jacket" 2 Survivor)
    { cdCardTraits = setFromList [Item, Clothing]
    , cdSkills = [#agility]
    , cdSlots = [#body]
    , cdLevel = Just 2
    }

marinersCompass2 :: CardDef
marinersCompass2 =
  (asset "10122" "Mariner's Compass" 2 Survivor)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

survivalTechnique2 :: CardDef
survivalTechnique2 =
  (asset "10123" "Survival Technique" 2 Survivor)
    { cdCardTraits = setFromList [Talent, Science]
    , cdSkills = [#agility]
    , cdLevel = Just 2
    }

tokenOfFaith3 :: CardDef
tokenOfFaith3 =
  (asset "10126" "Token of Faith" 2 Survivor)
    { cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdSkills = [#willpower, #intellect]
    , cdSlots = [#accessory]
    , cdLevel = Just 3
    }

darkHorse5 :: CardDef
darkHorse5 =
  permanent
    $ (asset "10127" "Dark Horse" 0 Survivor)
      { cdCardTraits = singleton Condition
      , cdDeckRestrictions = [PerDeckLimit 1]
      , cdLevel = Just 5
      }

eldritchTongue :: CardDef
eldritchTongue =
  (asset "10128" "Eldritch Tongue" 2 Neutral)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    }

occultReliquary3 :: CardDef
occultReliquary3 =
  permanent
    $ (asset "10132" ("Occult Reliquary" <:> "Dubious Source") 0 Neutral)
      { cdCardTraits = setFromList [Boon, Pact]
      , cdDeckRestrictions = [PerDeckLimit 1]
      , cdLevel = Just 3
      }

brokenDiademCrownOfDyingLight5 :: CardDef
brokenDiademCrownOfDyingLight5 =
  (asset "10133" ("Broken Diadem" <:> "Crown of Dying Light") 1 Neutral)
    { cdCardTraits = setFromList [Item, Charm, Mask]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdLimits = [LimitPerTrait Mask 1]
    , cdLevel = Just 5
    , cdUnique = True
    , cdBondedWith = [(1, "10134")]
    }

twilightDiademCrownOfDyingLight :: CardDef
twilightDiademCrownOfDyingLight =
  (asset "10134" ("Twilight Diadem" <:> "Crown of Dying Light") 0 Neutral)
    { cdCardTraits = setFromList [Item, Charm, Mask, Blessed]
    , cdCost = Nothing
    , cdLimits = [LimitPerTrait Mask 1]
    , cdUnique = True
    , cdKeywords = singleton (Keyword.Bonded 1 "10133")
    }

maimedHand :: CardDef
maimedHand =
  (basicWeakness "10135" "Maimed Hand")
    { cdCardTraits = setFromList [Injury]
    , cdSlots = [#hand]
    }

backInjury :: CardDef
backInjury =
  (basicWeakness "10136" "Back Injury")
    { cdCardTraits = setFromList [Injury]
    , cdSlots = [#body]
    }

theSilverMoth :: CardDef
theSilverMoth =
  (basicWeakness "10137" "The Silver Moth")
    { cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    }

vowOfDrzytelech :: CardDef
vowOfDrzytelech =
  (basicWeakness "10138" "Vow of Dryztelech")
    { cdCardTraits = setFromList [Pact]
    , cdSlots = [#arcane]
    }
