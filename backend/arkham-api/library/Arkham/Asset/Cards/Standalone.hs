module Arkham.Asset.Cards.Standalone where

import Arkham.Asset.Cards.Import

valeriyaAntonovaWantsOutOfHere :: CardDef
valeriyaAntonovaWantsOutOfHere =
  (storyAsset "71016" ("Valeriya Antonova" <:> "Wants Out of Here") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Leader, Guest, Agency]
    , cdSkills = [#willpower, #intellect, #combat]
    , cdUnique = True
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "71016b"
    }

archibaldHudson :: CardDef
archibaldHudson =
  (storyAsset "71017" ("Archibald Hudson" <:> "Astute Agent") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Agency]
    , cdSkills = [#willpower, #willpower, #intellect, #intellect]
    , cdSlots = [#ally]
    , cdUnique = True
    }

specialAgentCallahan :: CardDef
specialAgentCallahan =
  (storyAsset "71018" ("Special Agent Callahan" <:> "Weapons Expert") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Agency]
    , cdSkills = [#intellect, #intellect, #combat, #combat]
    , cdSlots = [#ally]
    , cdUnique = True
    }

horacioMartinez :: CardDef
horacioMartinez =
  (storyAsset "71019" ("Horacio Martinez" <:> "Brash Bodyguard") 3 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Agency]
    , cdSkills = [#combat, #combat, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

caldwellPhilipsEnthralledByLegends :: CardDef
caldwellPhilipsEnthralledByLegends =
  (storyAsset "71022" ("Caldwell Philips" <:> "Enthralled by Legends") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Leader, Guest, Miskatonic]
    , cdSkills = [#willpower, #intellect, #agility]
    , cdUnique = True
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "71022b"
    }

drMyaBadry :: CardDef
drMyaBadry =
  (storyAsset "71023" ("Dr. Mya Badry" <:> "Medical Examiner") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Miskatonic]
    , cdSkills = [#intellect, #intellect, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

lucasTetlow :: CardDef
lucasTetlow =
  (storyAsset "71024" ("Lucas Tetlow" <:> "Faculty Curator") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Miskatonic]
    , cdSkills = [#intellect, #intellect, #combat, #combat]
    , cdSlots = [#ally]
    , cdUnique = True
    }

elizabethConrad :: CardDef
elizabethConrad =
  (storyAsset "71025" ("Elizabeth Conrad" <:> "Completely Zozzled") 3 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Miskatonic]
    , cdSkills = [#willpower, #willpower, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

johnnyValoneReadyToMakeADeal :: CardDef
johnnyValoneReadyToMakeADeal =
  (storyAsset "71028" ("Johnny Valone" <:> "Ready to Make a Deal") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Leader, Guest, Syndicate]
    , cdSkills = [#intellect, #combat, #agility]
    , cdUnique = True
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "71028b"
    }

mirandaKeeper :: CardDef
mirandaKeeper =
  (storyAsset "71029" ("Miranda Keeper" <:> "Antiquities \"Trader\"") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Syndicate]
    , cdSkills = [#intellect, #intellect, #combat, #combat]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Supply 3
    }

arseneRenard :: CardDef
arseneRenard =
  (storyAsset "71030" ("Ars\xE8ne Renard" <:> "Gentleman Thief") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Syndicate]
    , cdSkills = [#intellect, #intellect, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

novaMalone :: CardDef
novaMalone =
  (storyAsset "71031" ("Nova Malone" <:> "Commanding Gangster") 3 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Syndicate]
    , cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdSlots = [#ally]
    , cdUnique = True
    }

carlSanfordLustingForPower :: CardDef
carlSanfordLustingForPower =
  (storyAsset "71034" ("Carl Sanford" <:> "Lusting for Power") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Leader, Guest, SilverTwilight]
    , cdSkills = [#willpower, #intellect, #combat]
    , cdUnique = True
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "71034b"
    }

prudenceDouglas :: CardDef
prudenceDouglas =
  (storyAsset "71035" ("Prudence Douglas" <:> "Pragmatic Occultist") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Sorcerer, SilverTwilight]
    , cdSkills = [#willpower, #willpower, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Portent 3
    }

sarahVanShaw :: CardDef
sarahVanShaw =
  (storyAsset "71036" ("Sarah Van Shaw" <:> "Lodge Warden") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, SilverTwilight]
    , cdSkills = [#combat, #combat, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

raymondLoggins :: CardDef
raymondLoggins =
  (storyAsset "71037" ("Raymond Loggins" <:> "Mysterious Benefactor") 3 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Sorcerer, SilverTwilight]
    , cdSkills = [#willpower, #willpower, #intellect, #intellect]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Truth 4
    }

williamBainLookingForThoseLost :: CardDef
williamBainLookingForThoseLost =
  (storyAsset "71040" ("William Bain" <:> "Looking for Those Lost") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Leader, Guest, Kingsport]
    , cdSkills = [#willpower, #combat, #agility]
    , cdUnique = True
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "71040b"
    }

deloresGadling :: CardDef
deloresGadling =
  (storyAsset "71041" ("Delores Gadling" <:> "Lantern Club Infiltrator") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Kingsport]
    , cdSkills = [#combat, #combat, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

thomasOlney :: CardDef
thomasOlney =
  (storyAsset "71042" ("Thomas Olney" <:> "Inquisitive Adventurer") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Wayfarer]
    , cdSkills = [#willpower, #willpower, #intellect, #intellect]
    , cdSlots = [#ally]
    , cdUnique = True
    }

claireWilson :: CardDef
claireWilson =
  (storyAsset "71043" ("Claire Wilson" <:> "Entirely Unimpressed") 2 TheMidwinterGala)
    { cdCardTraits = setFromList [Ally, Guest, Kingsport]
    , cdSkills = [#willpower, #willpower, #agility, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

thePaleLanternHypnoticGlow :: CardDef
thePaleLanternHypnoticGlow =
  (storyAsset "71046" ("The Pale Lantern" <:> "Hypnotic Glow") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Item, Relic]
    , cdCost = Nothing
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "71068b"
    }

thePaleLanternBeguilingAura :: CardDef
thePaleLanternBeguilingAura =
  (storyAsset "71046b" ("The Pale Lantern" <:> "Beguiling Aura") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Item, Relic]
    , cdCost = Nothing
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "71068"
    , cdVictoryPoints = Just 1
    }

jewelOfSarnath :: CardDef
jewelOfSarnath =
  (storyAsset "71052" ("Jewel of Sarnath" <:> "Turning Dreams Into Reality") 0 TheMidwinterGala)
    { cdCardTraits = setFromList [Item, Relic]
    , cdCost = Nothing
    , cdUnique = True
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

heliosTelescopeGateToTheCosmos :: CardDef
heliosTelescopeGateToTheCosmos =
  (storyAsset "72005" ("Helios Telescope" <:> "Gate to the Cosmos") 0 FilmFatale)
    { cdCardTraits = setFromList [Item, Relic, Prop, Cursed]
    , cdUnique = True
    , cdUses = uses Shard 0
    , cdVictoryPoints = Just 1
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

staffOfTheSerpentRelicOfThePast :: CardDef
staffOfTheSerpentRelicOfThePast =
  (storyAsset "72006" ("Staff of the Serpent" <:> "Relic of the Past") 0 FilmFatale)
    { cdCardTraits = setFromList [Item, Relic, Prop, Cursed]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

accursedCapeShroudOfChaos :: CardDef
accursedCapeShroudOfChaos =
  (storyAsset "72007" ("Accursed Cape" <:> "Shroud of Chaos") 0 FilmFatale)
    { cdCardTraits = setFromList [Item, Relic, Prop, Cursed]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

andrePatelMadeForTheSpotlight :: CardDef
andrePatelMadeForTheSpotlight =
  (storyAsset "72024" ("Andre Patel" <:> "Made for the Spotlight") 3 FilmFatale)
    { cdCardTraits = setFromList [Ally, Performer]
    , cdSkills = [#intellect, #agility, #wild]
    , cdUnique = True
    , cdSlots = [#ally]
    }

rocketShipRattlingWithEnergy :: CardDef
rocketShipRattlingWithEnergy =
  (storyAsset "72036" ("Rocket Ship" <:> "Rattling with Energy") 0 FilmFatale)
    { cdCardTraits = setFromList [Vehicle, Cosmos]
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "72036b"
    }

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
