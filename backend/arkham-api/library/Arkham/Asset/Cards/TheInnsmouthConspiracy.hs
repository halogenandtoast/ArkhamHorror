module Arkham.Asset.Cards.TheInnsmouthConspiracy where

import Arkham.Asset.Cards.Import
import Arkham.Criteria qualified as Criteria
import Arkham.Keyword (Sealing (..))
import Arkham.Keyword qualified as Keyword

guardianAngel :: CardDef
guardianAngel =
  signature "07001"
    $ (asset "07006" "Guardian Angel" 2 Neutral)
      { cdCardTraits = setFromList [Ritual, Blessed]
      , cdSkills = [#willpower, #combat, #wild]
      }

showmanship :: CardDef
showmanship =
  signature "07004"
    $ (asset "07012" "Showmanship" 1 Neutral)
      { cdCardTraits = setFromList [Talent]
      , cdSkills = [#combat, #agility, #wild]
      }

occultScraps :: CardDef
occultScraps =
  (weakness "07013" "Occult Scraps")
    { cdCardTraits = setFromList [Item]
    , cdCriteria = Just Criteria.Never
    , cdCardInHandEffects = True
    , cdCost = Just (StaticCost 0)
    }

seaChangeHarpoon :: CardDef
seaChangeHarpoon =
  signature "07005"
    $ (asset "07014" "Sea Change Harpoon" 3 Neutral)
      { cdCardTraits = setFromList [Item, Weapon, Melee]
      , cdSkills = [#combat, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

silassNet :: CardDef
silassNet =
  signature "07005"
    $ (asset "07015" "Silas's Net" 2 Neutral)
      { cdCardTraits = setFromList [Item, Tool]
      , cdSkills = [#agility, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

bookOfPsalms :: CardDef
bookOfPsalms =
  (asset "07017" "Book of Psalms" 3 Guardian)
    { cdCardTraits = setFromList [Item, Tome, Blessed]
    , cdSkills = [#willpower]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    }

blessedBlade :: CardDef
blessedBlade =
  (asset "07018" "Blessed Blade" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee, Blessed]
    , cdSkills = [#combat]
    , cdSlots = [#hand]
    }

riteOfSanctification :: CardDef
riteOfSanctification =
  (asset "07019" "Rite of Sanctification" 0 Guardian)
    { cdCardTraits = setFromList [Ritual, Blessed]
    , cdSkills = [#intellect]
    , cdSlots = [#arcane]
    , cdKeywords = singleton $ seal $ SealUpTo 5 #bless
    }

cryptographicCipher :: CardDef
cryptographicCipher =
  (asset "07021" "Cryptographic Cipher" 3 Seeker)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect]
    , cdSlots = [#hand]
    , cdUses = uses Secret 3
    }

crypticGrimoireUntranslated :: CardDef
crypticGrimoireUntranslated =
  (asset "07022" ("Cryptic Grimoire" <:> "Untranslated") 3 Seeker)
    { cdCardTraits = setFromList [Item, Tome, Occult]
    , cdSkills = [#wild]
    , cdSlots = [#hand]
    }

twentyFiveAutomatic :: CardDef
twentyFiveAutomatic =
  fast
    $ (asset "07025" ".25 Automatic" 4 Rogue)
      { cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
      , cdSkills = [#agility]
      , cdSlots = [#hand]
      , cdUses = uses Ammo 4
      }

darkRitual :: CardDef
darkRitual =
  (asset "07026" "Dark Ritual" 1 Rogue)
    { cdCardTraits = setFromList [Ritual, Cursed]
    , cdSkills = [#intellect]
    , cdSlots = [#arcane]
    , cdKeywords = singleton $ seal $ SealUpTo 5 #curse
    }

obfuscation :: CardDef
obfuscation =
  fast
    $ (asset "07027" "Obfuscation" 2 Rogue)
      { cdCardTraits = setFromList [Spell]
      , cdSkills = [#combat]
      , cdSlots = [#arcane]
      , cdUses = uses Charge 3
      }

swordCane :: CardDef
swordCane =
  (asset "07029" "Sword Cane" 2 Mystic)
    { cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdSkills = [#combat]
    , cdSlots = [#hand]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

tokenOfFaith :: CardDef
tokenOfFaith =
  (asset "07033" "Token of Faith" 2 Survivor)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSkills = [#intellect]
    , cdSlots = [#accessory]
    }

thomasDawsonSoldierInANewWar :: CardDef
thomasDawsonSoldierInANewWar =
  (storyAsset "07082" ("Thomas Dawson" <:> "Soldier in a New War") 4 TheVanishingOfElinaHarper)
    { cdSkills = [#willpower, #combat, #wild]
    , cdCardTraits = setFromList [Ally, Agency, Veteran]
    , cdUnique = True
    , cdSlots = [#ally]
    }

elinaHarperKnowsTooMuch :: CardDef
elinaHarperKnowsTooMuch =
  (storyAsset "07083" ("Elina Harper" <:> "Knows Too Much") 4 TheVanishingOfElinaHarper)
    { cdSkills = [#intellect, #agility, #wild]
    , cdCardTraits = setFromList [Ally, Agency, Detective]
    , cdUnique = True
    , cdSlots = [#ally]
    }

riotWhistle :: CardDef
riotWhistle =
  (asset "07108" "Riot Whistle" 2 Guardian)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#willpower]
    , cdSlots = [#accessory]
    }

sacredCovenant2 :: CardDef
sacredCovenant2 =
  permanent
    $ (asset "07110" "Sacred Covenant" 0 Guardian)
      { cdCardTraits = setFromList [Covenant, Blessed]
      , cdDeckRestrictions = [TraitPerDeckLimit Covenant 1]
      , cdLevel = Just 2
      }

eldritchSophist :: CardDef
eldritchSophist =
  (asset "07111" "Eldritch Sophist" 4 Seeker)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSkills = [#willpower]
    , cdSlots = [#ally]
    , cdUses = uses Secret 3
    }

blasphemousCovenant2 :: CardDef
blasphemousCovenant2 =
  permanent
    $ (asset "07113" "Blasphemous Covenant" 0 Seeker)
      { cdCardTraits = setFromList [Covenant, Cursed]
      , cdDeckRestrictions = [TraitPerDeckLimit Covenant 1]
      , cdLevel = Just 2
      }

falseCovenant2 :: CardDef
falseCovenant2 =
  permanent
    $ (asset "07116" "False Covenant" 0 Rogue)
      { cdCardTraits = setFromList [Covenant, Cursed]
      , cdDeckRestrictions = [TraitPerDeckLimit Covenant 1]
      , cdLevel = Just 2
      }

armageddon :: CardDef
armageddon =
  (asset "07117" "Armageddon" 4 Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdSkills = [#combat]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

eyeOfChaos :: CardDef
eyeOfChaos =
  (asset "07118" "Eye of Chaos" 5 Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdSkills = [#intellect]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

shroudOfShadows :: CardDef
shroudOfShadows =
  (asset "07119" "Shroud of Shadows" 3 Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdSkills = [#agility]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

paradoxicalCovenant2 :: CardDef
paradoxicalCovenant2 =
  permanent
    $ (asset "07120" "Paradoxical Covenant" 0 Mystic)
      { cdCardTraits = setFromList [Covenant, Blessed, Cursed]
      , cdDeckRestrictions = [TraitPerDeckLimit Covenant 1]
      , cdLevel = Just 2
      }

marinersCompass :: CardDef
marinersCompass =
  (asset "07121" "Mariner's Compass" 3 Survivor)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect]
    , cdSlots = [#hand]
    }

ancientCovenant2 :: CardDef
ancientCovenant2 =
  permanent
    $ (asset "07122" "Ancient Covenant" 0 Survivor)
      { cdCardTraits = setFromList [Covenant, Blessed]
      , cdDeckRestrictions = [TraitPerDeckLimit Covenant 1]
      , cdLevel = Just 2
      }

teachingsOfTheOrder :: CardDef
teachingsOfTheOrder =
  (storyAsset "07151" "Teachings of the Order" 0 InTooDeep)
    { cdCardTraits = setFromList [Item, Tome]
    , cdPermanent = True
    }

joeSargentRattletrapBusDriver :: CardDef
joeSargentRattletrapBusDriver =
  (storyAsset "07150" ("Joe Sargant" <:> "Rattletrap Bus Driver") 0 InTooDeep)
    { cdCardTraits = setFromList [Ally, Hybrid]
    , cdUnique = True
    , cdUses = uses Ticket 3
    }

keenEye :: CardDef
keenEye =
  (asset "07152" "Keen Eye" 2 Guardian)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#intellect, #combat]
    }

priestOfTwoFaiths1 :: CardDef
priestOfTwoFaiths1 =
  (asset "07156" "Priest of Two Faiths" 1 Rogue)
    { cdCardTraits = setFromList [Ally, Blessed, Cursed]
    , cdSkills = [#agility]
    , cdSlots = [#ally]
    , cdLevel = Just 1
    }

bloodPact :: CardDef
bloodPact =
  (asset "07158" "Blood Pact" 2 Mystic)
    { cdCardTraits = setFromList [Spell, Pact]
    , cdSkills = [#willpower, #combat]
    }

abyssalTome2 :: CardDef
abyssalTome2 =
  (asset "07159" "Abyssal Tome" 2 Mystic)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

fishingVessel :: CardDef
fishingVessel =
  (storyAsset "07178" "Fishing Vessel" 0 DevilReef)
    { cdCardTraits = singleton Vehicle
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

wavewornIdol :: CardDef
wavewornIdol =
  (storyAsset "07179" "Waveworn Idol" 2 DevilReef)
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    }

awakenedMantle :: CardDef
awakenedMantle =
  (storyAsset "07180" "Awakened Mantle" 1 DevilReef)
    { cdSkills = [#agility, #wild]
    , cdCardTraits = setFromList [Item, Relic, Clothing]
    }

headdressOfYhaNthlei :: CardDef
headdressOfYhaNthlei =
  (storyAsset "07181" "Headdress of Y'ha-nthlei" 1 DevilReef)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    }

enchantedArmor2 :: CardDef
enchantedArmor2 =
  (asset "07189" "Enchanted Armor" 1 Guardian)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Ritual, Armor]
    , cdSlots = [#body, #arcane]
    , cdLevel = Just 2
    }

blessingOfIsis3 :: CardDef
blessingOfIsis3 =
  (asset "07190" "Blessing of Isis" 2 Guardian)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Ritual, Blessed]
    , cdLevel = Just 3
    }

crypticGrimoireTextOfTheElderHerald4 :: CardDef
crypticGrimoireTextOfTheElderHerald4 =
  (asset "07191" ("Cryptic Grimoire" <:> "Text of the Elder Herald") 3 Seeker)
    { cdCardTraits = setFromList [Item, Tome, Cursed]
    , cdSkills = [#intellect, #agility]
    , cdSlots = [#hand]
    , cdLevel = Just 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheGrimoire
    }

crypticGrimoireTextOfTheElderGuardian4 :: CardDef
crypticGrimoireTextOfTheElderGuardian4 =
  (asset "07192" ("Cryptic Grimoire" <:> "Text of the Elder Guardian") 3 Seeker)
    { cdCardTraits = setFromList [Item, Tome, Blessed]
    , cdSkills = [#willpower, #intellect]
    , cdSlots = [#hand]
    , cdLevel = Just 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheGrimoire
    }

tristanBotleyFixerForHire2 :: CardDef
tristanBotleyFixerForHire2 =
  (asset "07194" ("Tristan Botley" <:> "Fixer for Hire") 5 Rogue)
    { cdCardTraits = setFromList [Ally, Criminal, Cursed]
    , cdSkills = [#wild]
    , cdSlots = [#ally]
    , cdLevel = Just 2
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheGrimoire
    , cdCardInHandEffects = True
    , cdUnique = True
    }

curseOfAeons3 :: CardDef
curseOfAeons3 =
  (asset "07195" "Curse of Aeons" 2 Mystic)
    { cdCardTraits = setFromList [Ritual, Cursed]
    , cdSkills = [#wild]
    , cdLevel = Just 3
    }

thomasDawsonsCarRunning :: CardDef
thomasDawsonsCarRunning =
  (storyAsset "07211a" ("Thomas Dawson's Car" <:> "Running") 0 HorrorInHighGear)
    { cdCardTraits = singleton Vehicle
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdOtherSide = Just "07211b"
    }

thomasDawsonsCarStopped :: CardDef
thomasDawsonsCarStopped =
  (storyAsset "07211b" ("Thomas Dawson's Car" <:> "Stopped") 0 HorrorInHighGear)
    { cdCardTraits = singleton Vehicle
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdOtherSide = Just "07211a"
    }

elinaHarpersCarRunning :: CardDef
elinaHarpersCarRunning =
  (storyAsset "07212a" ("Elina Harper's Car" <:> "Running") 0 HorrorInHighGear)
    { cdCardTraits = singleton Vehicle
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdOtherSide = Just "07212b"
    }

elinaHarpersCarStopped :: CardDef
elinaHarpersCarStopped =
  (storyAsset "07212b" ("Elina Harper's Car" <:> "Stopped") 0 HorrorInHighGear)
    { cdCardTraits = singleton Vehicle
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdOtherSide = Just "07212a"
    }

holyRosary2 :: CardDef
holyRosary2 =
  (asset "07220" "Holy Rosary" 2 Guardian)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdSlots = [#accessory]
    , cdLevel = Just 2
    }

shieldOfFaith2 :: CardDef
shieldOfFaith2 =
  (asset "07221" "Shield of Faith" 2 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdSlots = [#arcane]
    , cdLevel = Just 2
    , cdKeywords = singleton $ seal $ SealUpTo 5 #bless
    }

guidedByTheUnseen3 :: CardDef
guidedByTheUnseen3 =
  (asset "07223" "Guided by the Unseen" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdLevel = Just 3
    , cdUses = uses Secret 4
    }

luckyPennyOmenOfMisfortune2 :: CardDef
luckyPennyOmenOfMisfortune2 =
  (asset "07224" ("\"Lucky\" Penny" <:> "Omen of Misfortune") 2 Rogue)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdSlots = [#accessory]
    , cdLevel = Just 2
    , cdKeywords = setFromList [Keyword.Exceptional]
    , cdUnique = True
    }

eyeOfTheDjinnVesselOfGoodAndEvil2 :: CardDef
eyeOfTheDjinnVesselOfGoodAndEvil2 =
  (asset "07225" ("Eye of the Djinn" <:> "Vessel of Good and Evil") 2 Rogue)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Relic, Blessed, Cursed]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    , cdKeywords = setFromList [Keyword.Exceptional]
    , cdUnique = True
    }

armageddon4 :: CardDef
armageddon4 =
  (asset "07226" "Armageddon" 4 Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdSkills = [#willpower, #combat]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = Just 4
    }

eyeOfChaos4 :: CardDef
eyeOfChaos4 =
  (asset "07227" "Eye of Chaos" 5 Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdSkills = [#willpower, #intellect]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = Just 4
    }

shroudOfShadows4 :: CardDef
shroudOfShadows4 =
  (asset "07228" "Shroud of Shadows" 3 Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdSkills = [#willpower, #agility]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = Just 4
    }

spiritOfHumanity2 :: CardDef
spiritOfHumanity2 =
  (asset "07229" "Spirit of Humanity" 2 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual, Blessed, Cursed]
    , cdSlots = [#arcane]
    , cdLevel = Just 2
    }

nephthysHuntressOfBast4 :: CardDef
nephthysHuntressOfBast4 =
  (asset "07262" ("Nephthys" <:> "Huntress of Bast") 3 Guardian)
    { cdSkills = [#willpower, #combat, #combat]
    , cdCardTraits = setFromList [Ally, Blessed]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 4
    }

hyperawareness4 :: CardDef
hyperawareness4 =
  (asset "07264" "Hyperawareness" 2 Seeker)
    { cdSkills = [#intellect, #intellect, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdUses = uses Resource 2
    , cdLevel = Just 4
    }

geas2 :: CardDef
geas2 =
  (asset "07265" "Geas" 2 Rogue)
    { cdCardTraits = setFromList [Pact]
    , cdKeywords = setFromList [Keyword.Exceptional]
    , cdLevel = Just 2
    }

hardKnocks4 :: CardDef
hardKnocks4 =
  (asset "07266" "Hard Knocks" 2 Rogue)
    { cdSkills = [#combat, #combat, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdUses = uses Resource 2
    , cdLevel = Just 4
    }

ikiaqTheCouncilsChosen3 :: CardDef
ikiaqTheCouncilsChosen3 =
  (asset "07267" ("Ikiaq" <:> "The Council's Chosen") 3 Mystic)
    { cdCardTraits = setFromList [Ally, Sorcerer]
    , cdSkills = [#willpower, #intellect]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdLevel = Just 3
    }

fluteOfTheOuterGods4 :: CardDef
fluteOfTheOuterGods4 =
  (asset "07268" "Flute of the Outer Gods" 0 Mystic)
    { cdCardTraits = setFromList [Item, Instrument, Relic, Cursed]
    , cdSkills = [#willpower, #combat, #agility]
    , cdUnique = True
    , cdSlots = [#hand]
    , cdKeywords = setFromList [Keyword.Exceptional, seal $ SealUpToX #curse]
    , cdLevel = Just 4
    , cdCost = Just DynamicCost
    }

purifyingCorruption4 :: CardDef
purifyingCorruption4 =
  (asset "07273" "Purifying Corruption" 4 Neutral)
    { cdCardTraits = setFromList [Ritual, Blessed, Cursed]
    , cdSkills = [#wild]
    , cdLevel = Just 4
    }

digDeep4 :: CardDef
digDeep4 =
  (asset "07270" "Dig Deep" 2 Survivor)
    { cdSkills = [#willpower, #willpower, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdUses = uses Resource 2
    , cdLevel = Just 4
    }

favorOfTheMoon1 :: CardDef
favorOfTheMoon1 =
  fast
    $ (asset "07271" "Favor of the Moon" 1 Neutral)
      { cdCardTraits = setFromList [Pact, Cursed]
      , cdSkills = [#intellect, #combat]
      , cdKeywords = singleton $ seal $ SealUpTo 3 #curse
      , cdLevel = Just 1
      , cdUnique = True
      }

favorOfTheSun1 :: CardDef
favorOfTheSun1 =
  fast
    $ (asset "07272" "Favor of the Sun" 2 Neutral)
      { cdCardTraits = setFromList [Pact, Blessed]
      , cdSkills = [#willpower, #agility]
      , cdKeywords = singleton $ seal $ SealUpTo 3 #bless
      , cdLevel = Just 1
      , cdUnique = True
      }

yhanthleiStatueMysteriousRelic :: CardDef
yhanthleiStatueMysteriousRelic =
  (storyAsset "07300" ("Y'ha-nthlei Status" <:> "Mysterious Relic") 1 TheLairOfDagon)
    { cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "07300b"
    , cdCardType = EncounterAssetType
    }

yhanthleiStatueDynamicRelic :: CardDef
yhanthleiStatueDynamicRelic =
  (storyAsset "07300b" ("Y'ha-nthlei Status" <:> "Dynamic Relic") 1 TheLairOfDagon)
    { cdCardTraits = setFromList [Item, Relic, Blessed]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "07300"
    , cdCardType = EncounterAssetType
    }

holySpear5 :: CardDef
holySpear5 =
  (asset "07302" "Holy Spear" 4 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee, Blessed]
    , cdSkills = [#willpower, #combat, #combat]
    , cdSlots = [#hand, #hand]
    , cdLevel = Just 5
    }

ancestralKnowledge3 :: CardDef
ancestralKnowledge3 =
  permanent
    $ (asset "07303" "Ancestral Knowledge" 0 Seeker)
      { cdCardTraits = singleton Talent
      , cdKeywords = setFromList [Keyword.Exceptional]
      , cdLevel = Just 3
      }

ariadnesTwine3 :: CardDef
ariadnesTwine3 =
  (asset "07304" "Ariadne's Twine" 0 Seeker)
    { cdCardTraits = setFromList [Ritual]
    , cdUses = uses Secret 0
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#arcane]
    , cdLevel = Just 3
    }

twentyFiveAutomatic2 :: CardDef
twentyFiveAutomatic2 =
  fast
    $ (asset "07305" ".25 Automatic" 4 Rogue)
      { cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
      , cdSkills = [#combat, #agility]
      , cdSlots = [#hand]
      , cdUses = uses Ammo 4
      , cdLevel = Just 2
      }

luckyDice3 :: CardDef
luckyDice3 =
  (asset "07307" ("Lucky Dice" <:> "...Or Are They?") 2 Rogue)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#willpower, #agility]
    , cdExceptional = True
    , cdLevel = Just 3
    , cdSlots = [#accessory]
    }

jacobMorrisonCostGuardCaptain3 :: CardDef
jacobMorrisonCostGuardCaptain3 =
  (asset "07309" ("Jacob Morrison" <:> "Coast Guard Captain") 3 Survivor)
    { cdCardTraits = setFromList [Ally, Blessed]
    , cdSkills = [#wild]
    , cdSlots = [#ally]
    , cdLevel = Just 3
    , cdUnique = True
    }

divingSuit :: CardDef
divingSuit =
  (storyAsset "07338" "Diving Suit" 4 IntoTheMaelstrom)
    { cdCardTraits = setFromList [Item, Armor]
    , cdCardType = EncounterAssetType
    }
