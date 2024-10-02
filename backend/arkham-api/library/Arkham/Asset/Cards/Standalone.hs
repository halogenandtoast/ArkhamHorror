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
