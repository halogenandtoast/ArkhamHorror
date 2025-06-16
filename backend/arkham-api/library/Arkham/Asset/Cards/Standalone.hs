module Arkham.Asset.Cards.Standalone where

import Arkham.Asset.Cards.Import

ladyEsprit :: CardDef
ladyEsprit =
  (storyAsset "81019" ("Lady Esprit" <:> "Dangerous Bokor") 4 TheBayou)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Sorcerer]
    , cdUnique = True
    , cdSlots = [#ally]
    }

bearTrap :: CardDef
bearTrap =
  (storyAsset "81020" "Bear Trap" 0 TheBayou)
    { cdCardTraits = setFromList [Trap]
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

fishingNet :: CardDef
fishingNet =
  (storyAsset "81021" "Fishing Net" 0 TheBayou)
    { cdCardTraits = setFromList [Trap]
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

monstrousTransformation :: CardDef
monstrousTransformation =
  fast
    $ (storyAsset "81030" "Monstrous Transformation" 0 CurseOfTheRougarou)
      { cdCardTraits = setFromList [Talent]
      }

maskedCarnevaleGoer_17 :: CardDef
maskedCarnevaleGoer_17 =
  (storyAsset "82017b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    }

maskedCarnevaleGoer_18 :: CardDef
maskedCarnevaleGoer_18 =
  (storyAsset "82018b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    }

maskedCarnevaleGoer_19 :: CardDef
maskedCarnevaleGoer_19 =
  (storyAsset "82019b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    }

maskedCarnevaleGoer_20 :: CardDef
maskedCarnevaleGoer_20 =
  (storyAsset "82020b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    }

innocentReveler :: CardDef
innocentReveler =
  (storyAssetWithMany "82021" "Innocent Reveler" 0 CarnevaleOfHorrors 3)
    { cdCardTraits = setFromList [Ally, Bystander, Carnevale]
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "82021b"
    }

maskedCarnevaleGoer_21 :: CardDef
maskedCarnevaleGoer_21 =
  (storyAsset "82021b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "82021"
    }

abbessAllegriaDiBiase :: CardDef
abbessAllegriaDiBiase =
  ( storyAsset
      "82022"
      ("Abbess Allegria Di Biase" <:> "Most Blessed")
      4
      CarnevaleOfHorrors
  )
    { cdCardTraits = setFromList [Ally, Believer]
    , cdUnique = True
    , cdSkills = [#willpower, #intellect, #wild]
    , cdSlots = [#ally]
    }

bauta :: CardDef
bauta =
  (storyAsset "82023" "Bauta" 1 CarnevaleOfHorrors)
    { cdCardTraits = setFromList [Item, Mask]
    , cdSkills = [#combat, #wild]
    , cdLimits = [LimitPerTrait Mask 1]
    }

medicoDellaPeste :: CardDef
medicoDellaPeste =
  (storyAsset "82024" "Medico Della Peste" 1 CarnevaleOfHorrors)
    { cdCardTraits = setFromList [Item, Mask]
    , cdSkills = [#willpower, #wild]
    , cdLimits = [LimitPerTrait Mask 1]
    }

pantalone :: CardDef
pantalone =
  (storyAsset "82025" "Pantalone" 1 CarnevaleOfHorrors)
    { cdCardTraits = setFromList [Item, Mask]
    , cdSkills = [#intellect, #wild]
    , cdLimits = [LimitPerTrait Mask 1]
    }

gildedVolto :: CardDef
gildedVolto =
  (storyAsset "82026" "Gilded Volto" 1 CarnevaleOfHorrors)
    { cdCardTraits = setFromList [Item, Mask]
    , cdSkills = [#agility, #wild]
    , cdLimits = [LimitPerTrait Mask 1]
    }

bloodstainedDagger :: CardDef
bloodstainedDagger =
  (storyAsset "84006" ("Bloodstained Dagger" <:> "The Murder Weapon") 1 MurderAtTheExcelsiorHotel)
    { cdCardTraits = setFromList [Item, Weapon, Melee, Cursed]
    , cdSkills = [#wild]
    , cdSlots = [#hand]
    , cdUnique = True
    }

sergeantMonroe :: CardDef
sergeantMonroe =
  (storyAsset "84008" ("Sergeant Monroe" <:> "Two Days Until Retirement") 5 MurderAtTheExcelsiorHotel)
    { cdCardTraits = setFromList [Ally, Police]
    , cdSkills = [#willpower, #combat, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    }

alienDevice :: CardDef
alienDevice =
  (storyAsset "84028" ("Alien Device" <:> "Machinations from Beyond") 0 AlienInterference)
    { cdCardTraits = setFromList [Lead, Extraterrestrial]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

managersKey :: CardDef
managersKey =
  (storyAsset "84031" ("Manager's Key" <:> "Stained by Blood") 0 ExcelsiorManagement)
    { cdCardTraits = setFromList [Lead, Key]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

tomeOfRituals :: CardDef
tomeOfRituals =
  (storyAsset "84034" ("Tome of Rituals" <:> "Blasphemous Volume") 0 DarkRituals)
    { cdCardTraits = setFromList [Lead, Tome]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

sinisterSolution :: CardDef
sinisterSolution =
  (storyAsset "84037" ("Sinister Solution" <:> "Vile Concoction") 0 VileExperiments)
    { cdCardTraits = setFromList [Lead, Science]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

timeWornLocket :: CardDef
timeWornLocket =
  (storyAsset "84040" ("Time-Worn Locket" <:> "Mournful Vision of the Past") 0 SinsOfThePast)
    { cdCardTraits = setFromList [Lead, Charm]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

ravenousControlledHunger :: CardDef
ravenousControlledHunger =
  signature "89001"
    $ permanent
    $ (asset "89002" ("Ravenous" <:> "Controlled Hunger") 0 Neutral)
      { cdCardTraits = setFromList [Talent]
      , cdOtherSide = Just "89002b"
      }

ravenousUncontrolledHunger :: CardDef
ravenousUncontrolledHunger =
  signature "89001"
    $ permanent
    $ (weakness "89002b" ("Ravenous" <:> "Uncontrolled Hunger"))
      { cdCardTraits = setFromList [Flaw]
      , cdOtherSide = Just "89002"
      }

archibaldHudsonCard :: CardDef
archibaldHudsonCard =
  (storyAsset "71017" ("Archibald Hudson" <:> "Astute Agent") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Agency]
    , cdSkills = [#willpower, #willpower, #intellect, #intellect]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdHealth = Just 2
    , cdSanity = Just 2
    }

specialAgentCallahanCard :: CardDef
specialAgentCallahanCard =
  (storyAsset "71018" ("Special Agent Callahan" <:> "Weapons Expert") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Agency]
    , cdSkills = [#combat, #combat, #intellect, #intellect]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdHealth = Just 3
    , cdSanity = Just 1
    }

horacioMartinezCard :: CardDef
horacioMartinezCard =
  (storyAsset "71019" ("Horacio Martinez" <:> "Brash Bodyguard") 3 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Agency]
    , cdSkills = [#combat, #combat, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdHealth = Just 4
    , cdSanity = Just 1
    }

drMyaBadryCard :: CardDef
drMyaBadryCard =
  (storyAsset "71023" ("Dr. Mya Badry" <:> "Medical Examiner") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Miskatonic]
    , cdSkills = [#intellect, #intellect, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdHealth = Just 2
    , cdSanity = Just 2
    }

lucasTetlowCard :: CardDef
lucasTetlowCard =
  (storyAsset "71024" ("Lucas Tetlow" <:> "Faculty Curator") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Miskatonic]
    , cdSkills = [#intellect, #intellect, #combat, #combat]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdHealth = Just 3
    , cdSanity = Just 1
    }

elizabethConradCard :: CardDef
elizabethConradCard =
  (storyAsset "71025" ("Elizabeth Conrad" <:> "Completely Zozzled") 3 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Miskatonic]
    , cdSkills = [#willpower, #willpower, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdHealth = Just 1
    , cdSanity = Just 3
    }

mirandaKeeperCard :: CardDef
mirandaKeeperCard =
  (storyAsset "71026" ("Miranda Keeper" <:> "Antiquities \"Trader\"") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Syndicate]
    , cdSkills = [#intellect, #intellect, #combat, #combat]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Supply 3
    , cdHealth = Just 2
    , cdSanity = Just 2
    }

arseneRenardCard :: CardDef
arseneRenardCard =
  (storyAsset "71027" ("Ars\xE8ne Renard" <:> "Gentleman Thief") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Syndicate]
    , cdSkills = [#intellect, #intellect, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdHealth = Just 1
    , cdSanity = Just 3
    }

novaMaloneCard :: CardDef
novaMaloneCard =
  (storyAsset "71028" ("Nova Malone" <:> "Commanding Gangster") 3 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Syndicate]
    , cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdHealth = Just 3
    , cdSanity = Just 1
    }

prudenceDouglasCard :: CardDef
prudenceDouglasCard =
  (storyAsset "71029" ("Prudence Douglas" <:> "Pragmatic Occultist") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Sorcerer, SilverTwilight]
    , cdSkills = [#willpower, #willpower, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Portent 3
    , cdHealth = Just 2
    , cdSanity = Just 2
    }

sarahVanShawCard :: CardDef
sarahVanShawCard =
  (storyAsset "71030" ("Sarah Van Shaw" <:> "Lodge Warden") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, SilverTwilight]
    , cdSkills = [#combat, #combat, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdHealth = Just 3
    , cdSanity = Just 1
    }

raymondLogginsCard :: CardDef
raymondLogginsCard =
  (storyAsset "71031" ("Raymond Loggins" <:> "Mysterious Benefactor") 3 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Sorcerer, SilverTwilight]
    , cdSkills = [#willpower, #willpower, #intellect, #intellect]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Truth 4
    , cdHealth = Just 1
    , cdSanity = Just 2
    }

caldwellPhilipsEnthralledByLegendsCard :: CardDef
caldwellPhilipsEnthralledByLegendsCard =
  (storyAsset "71063b" ("Caldwell Philips" <:> "Enthralled by Legends") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Leader, Guest, Miskatonic]
    , cdSkills = [#willpower, #intellect, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdCost = Nothing
    , cdHealth = Just 2
    , cdSanity = Just 2
    }

carlSanfordLustingForPowerCard :: CardDef
carlSanfordLustingForPowerCard =
  (storyAsset "71064b" ("Carl Sanford" <:> "Lusting for Power") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Leader, Guest, SilverTwilight]
    , cdSkills = [#willpower, #intellect, #combat]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdCost = Nothing
    , cdHealth = Just 3
    , cdSanity = Just 3
    }

valeriyaAntonovaWantsOutOfHereCard :: CardDef
valeriyaAntonovaWantsOutOfHereCard =
  (storyAsset "71065b" ("Valeriya Antonova" <:> "Wants Out of Here") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Leader, Guest, Agency]
    , cdSkills = [#willpower, #intellect, #combat]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdCost = Nothing
    , cdHealth = Just 2
    , cdSanity = Just 3
    }

johnnyValoneReadyToMakeADealCard :: CardDef
johnnyValoneReadyToMakeADealCard =
  (storyAsset "71066b" ("Johnny Valone" <:> "Ready to Make a Deal") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Leader, Guest, Syndicate]
    , cdSkills = [#intellect, #combat, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdCost = Nothing
    , cdHealth = Just 3
    , cdSanity = Just 2
    }

jewelOfSarnathCard :: CardDef
jewelOfSarnathCard =
  (storyAsset "71067b" ("Jewel of Sarnath" <:> "Turning Dreams Into Reality") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Item, Relic]
    , cdCost = Nothing
    , cdUnique = True
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

thePaleLanternHypnoticGlowCard :: CardDef
thePaleLanternHypnoticGlowCard =
  (storyAsset "71068" ("The Pale Lantern" <:> "Hypnotic Glow") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Item, Relic]
    , cdCost = Nothing
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "71068b"
    }

thePaleLanternBeguilingAuraCard :: CardDef
thePaleLanternBeguilingAuraCard =
  (storyAsset "71068b" ("The Pale Lantern" <:> "Beguiling Aura") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Item, Relic]
    , cdCost = Nothing
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "71068"
    , cdVictoryPoints = Just 1
    }
