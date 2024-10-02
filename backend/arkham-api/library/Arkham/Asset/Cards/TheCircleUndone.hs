module Arkham.Asset.Cards.TheCircleUndone where

import Arkham.Asset.Cards.Import

hypnoticTherapy :: CardDef
hypnoticTherapy =
  signature "05001"
    $ (asset "05007" "Hypnotic Therapy" 2 Neutral)
      { cdCardTraits = singleton Talent
      , cdSkills = [#willpower, #intellect, #wild]
      }

detectivesColt1911s :: CardDef
detectivesColt1911s =
  signature "05002"
    $ (asset "05009" "Detective's Colt 1911s" 4 Neutral)
      { cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdSkills = [#intellect, #combat, #wild]
      , cdSlots = [#hand, #hand]
      , cdUses = uses Ammo 4
      }

familyInheritance :: CardDef
familyInheritance =
  permanent
    . signature "05003"
    $ (asset "05011" ("Family Inheritance" <:> "A Windfall? Or a Burden?") 0 Neutral)
      { cdCardTraits = singleton Boon
      , cdUnique = True
      }

twilightBlade :: CardDef
twilightBlade =
  signature "05004"
    $ (asset "05013" ("Twilight Blade" <:> "Sanctum's Reward") 3 Neutral)
      { cdCardTraits = setFromList [Item, Relic, Weapon]
      , cdSkills = [#willpower, #combat, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

baronSamedi :: CardDef
baronSamedi =
  (weakness "05019" ("Baron Samedi" <:> "Lord of the Cemetery"))
    { cdCardTraits = singleton Avatar
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["99003"]
    , cdUnique = True
    }

aceOfSwords1 :: CardDef
aceOfSwords1 =
  (asset "05023" ("Ace of Swords" <:> "Let Your Arrow Fly True") 3 Guardian)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 1
    , cdCardInHandEffects = True
    }

fingerprintKit :: CardDef
fingerprintKit =
  (asset "05024" "Fingerprint Kit" 4 Seeker)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect]
    , cdSlots = [#hand]
    , cdUses = uses Supply 3
    }

deathXiii1 :: CardDef
deathXiii1 =
  (asset "05027" ("Death • XIII" <:> "Free from the Past") 3 Seeker)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 1
    , cdCardInHandEffects = True
    }

wellConnected :: CardDef
wellConnected =
  (asset "05028" "Well Connected" 2 Rogue)
    { cdCardTraits = singleton Condition
    , cdSkills = [#intellect]
    , cdLimits = [LimitPerInvestigator 1]
    }

theMoonXiii1 :: CardDef
theMoonXiii1 =
  (asset "05031" ("The Moon • XVIII" <:> "Message from Your Inner Self") 3 Rogue)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 1
    , cdCardInHandEffects = True
    }

fourOfCups1 :: CardDef
fourOfCups1 =
  (asset "05035" ("Four of Cups" <:> "Chalice of the Heart") 3 Mystic)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 1
    , cdCardInHandEffects = True
    }

trackShoes :: CardDef
trackShoes =
  (asset "05036" "Track Shoes" 3 Survivor)
    { cdCardTraits = setFromList [Item, Clothing, Footwear]
    , cdSkills = [#agility]
    , cdLimits = [LimitPerTrait Footwear 1]
    }

fiveOfPentacles1 :: CardDef
fiveOfPentacles1 =
  (asset "05039" ("Five of Pentacles" <:> "From the Brink") 3 Survivor)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 1
    , cdCardInHandEffects = True
    }

aceOfRods1 :: CardDef
aceOfRods1 =
  (asset "05040" ("Ace of Rods" <:> "The Fateful Step") 3 Neutral)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 1
    , cdCardInHandEffects = True
    }

theTowerXVI :: CardDef
theTowerXVI =
  (basicWeakness "05042" ("The Tower • XVI" <:> "Circumstances Beyond Your Control"))
    { cdCardTraits = setFromList [Omen, Tarot]
    , cdSlots = [#tarot]
    , cdCardInHandEffects = True
    , cdCanReplace = False
    , cdRevelation = NoRevelation
    , cdCost = Just (StaticCost 4)
    }

somethingWorthFightingFor :: CardDef
somethingWorthFightingFor =
  (asset "05109" "Something Worth Fighting For" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    }

signMagick :: CardDef
signMagick =
  fast
    (asset "05112" "Sign Magick" 3 Mystic)
      { cdSkills = [#willpower]
      , cdCardTraits = setFromList [Ritual, Talent]
      , cdSlots = [#hand]
      }

meatCleaver :: CardDef
meatCleaver =
  (asset "05114" "Meat Cleaver" 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

fortyFiveThompson :: CardDef
fortyFiveThompson =
  (multiClassAsset "05115" ".45 Thompson" 6 [Guardian, Rogue])
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 5
    }

scrollOfSecrets :: CardDef
scrollOfSecrets =
  (multiClassAsset "05116" "Scroll of Secrets" 1 [Seeker, Mystic])
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 3
    }

tennesseeSourMash :: CardDef
tennesseeSourMash =
  (multiClassAsset "05117" "Tennessee Sour Mash" 3 [Rogue, Survivor])
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 2
    }

enchantedBlade :: CardDef
enchantedBlade =
  (multiClassAsset "05118" "Enchanted Blade" 3 [Mystic, Guardian])
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdUses = uses Charge 3
    , cdSlots = [#hand, #arcane]
    }

grislyTotem :: CardDef
grislyTotem =
  (multiClassAsset "05119" "Grisly Totem" 3 [Survivor, Seeker])
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    }

theBlackBook :: CardDef
theBlackBook =
  (storyAsset "05150" ("The Black Book" <:> "Signed in Blood") 3 TheSecretName)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Item, Tome, Relic]
    , cdSlots = [#hand]
    , cdUnique = True
    }

aliceLuxley :: CardDef
aliceLuxley =
  (asset "05151" ("Alice Luxley" <:> "Fearless Flatfoot") 4 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Detective, Police]
    , cdSlots = [#ally]
    , cdUnique = True
    }

mrRook :: CardDef
mrRook =
  (asset "05153" ("Mr. \"Rook\"" <:> "Dealer in Secrets") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Ally
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Secret 3
    }

hawkEyeFoldingCamera :: CardDef
hawkEyeFoldingCamera =
  (asset "05154" "Hawk-Eye Folding Camera" 2 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    }

henryWan :: CardDef
henryWan =
  (asset "05155" ("Henry Wan" <:> "Aspiring Actor") 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdSlots = [#ally]
    , cdUnique = True
    }

wither :: CardDef
wither =
  (asset "05157" "Wither" 2 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    }

sixthSense :: CardDef
sixthSense =
  (asset "05158" "Sixth Sense" 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    }

drawingThin :: CardDef
drawingThin =
  (asset "05159" "Drawing Thin" 0 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    }

spectralWeb :: CardDef
spectralWeb =
  (storyAssetWithMany "05177" "Spectral Web" 0 TheWagesOfSin 4)
    { cdCardTraits = singleton Spell
    }

fortyFiveThompsonGuardian3 :: CardDef
fortyFiveThompsonGuardian3 =
  (asset "05186" ".45 Thompson" 6 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 5
    , cdLevel = Just 3
    }

fortyFiveThompsonRogue3 :: CardDef
fortyFiveThompsonRogue3 =
  (asset "05187" ".45 Thompson" 5 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 5
    , cdLevel = Just 3
    }

scrollOfSecretsSeeker3 :: CardDef
scrollOfSecretsSeeker3 =
  (asset "05188" "Scroll of Secrets" 1 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 3
    , cdLevel = Just 3
    }

scrollOfSecretsMystic3 :: CardDef
scrollOfSecretsMystic3 =
  (asset "05189" "Scroll of Secrets" 1 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    , cdLevel = Just 3
    }

tennesseeSourMashRogue3 :: CardDef
tennesseeSourMashRogue3 =
  (asset "05190" "Tennessee Sour Mash" 3 Rogue)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 2
    , cdLevel = Just 3
    }

tennesseeSourMashSurvivor3 :: CardDef
tennesseeSourMashSurvivor3 =
  (asset "05191" "Tennessee Sour Mash" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 3
    , cdLevel = Just 3
    }

enchantedBladeGuardian3 :: CardDef
enchantedBladeGuardian3 =
  (asset "05192" "Enchanted Blade" 3 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdUses = uses Charge 3
    , cdSlots = [#hand, #arcane]
    , cdLevel = Just 3
    }

enchantedBladeMystic3 :: CardDef
enchantedBladeMystic3 =
  (asset "05193" "Enchanted Blade" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdUses = uses Charge 4
    , cdSlots = [#hand, #arcane]
    , cdLevel = Just 3
    }

grislyTotemSeeker3 :: CardDef
grislyTotemSeeker3 =
  (asset "05194" "Grisly Totem" 3 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdSlots = [#accessory]
    , cdLevel = Just 3
    }

grislyTotemSurvivor3 :: CardDef
grislyTotemSurvivor3 =
  (asset "05195" "Grisly Totem" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdSlots = [#accessory]
    , cdLevel = Just 3
    }

theCouncilsCoffer2 :: CardDef
theCouncilsCoffer2 =
  (asset "05196" ("The Council's Coffer" <:> "What's in the Box?") 0 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = Just 2
    , cdUses = Uses Lock (GameValueCalculation $ PerPlayer 1)
    , cdUnique = True
    }

augustLindquist :: CardDef
augustLindquist =
  (storyAsset "05227" ("August Lindquist" <:> "Elegant and Elusive") 0 ForTheGreaterGood)
    { cdCardTraits = setFromList [Cultist, SilverTwilight]
    , cdUnique = True
    , cdCost = Nothing
    }

puzzleBox :: CardDef
puzzleBox =
  (storyAsset "05228" ("Puzzle Box" <:> "Mysterious Device") 0 ForTheGreaterGood)
    { cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

esotericAtlas1 :: CardDef
esotericAtlas1 =
  (asset "05232" "Esoteric Atlas" 3 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    , cdLevel = Just 1
    }

investments :: CardDef
investments =
  (asset "05233" "Investments" 1 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Connection
    , cdUses = UsesWithLimit Supply (Fixed 0) (Fixed 10)
    }

deVermisMysteriis2 :: CardDef
deVermisMysteriis2 =
  (asset "05235" ("De Vermis Mysteriis" <:> "Signs of the Black Stars") 2 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdLevel = Just 2
    }

guidingSpirit1 :: CardDef
guidingSpirit1 =
  (asset "05236" "Guiding Spirit" 1 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Geist]
    , cdSlots = [#ally]
    , cdLevel = Just 1
    }

gavriellaMizrah :: CardDef
gavriellaMizrah =
  (storyAsset "05258" ("Gavriella Mizrah" <:> "Not Going Down That Easily") 2 ForTheGreaterGood)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = setFromList [Ally, Veteran]
    , cdUnique = True
    }

jeromeDavids :: CardDef
jeromeDavids =
  (storyAsset "05259" ("Jerome Davids" <:> "In Way Over His Head") 2 ForTheGreaterGood)
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = setFromList [Ally, Assistant]
    , cdUnique = True
    }

pennyWhite :: CardDef
pennyWhite =
  (storyAsset "05260" ("Penny White" <:> "The Nightmare is Over") 2 ForTheGreaterGood)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Ally, Assistant]
    , cdUnique = True
    }

valentinoRivas :: CardDef
valentinoRivas =
  (storyAsset "05261" ("Valentino Rivas" <:> "Took You Long Enough") 2 ForTheGreaterGood)
    { cdSkills = [#agility, #wild]
    , cdCardTraits = setFromList [Ally, Socialite]
    , cdUnique = True
    }

mk1Grenades4 :: CardDef
mk1Grenades4 =
  (asset "05273" "Mk 1 Grenades" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Ranged]
    , cdUses = uses Supply 3
    , cdLevel = Just 4
    }

agencyBackup5 :: CardDef
agencyBackup5 =
  (asset "05274" "Agency Backup" 7 Guardian)
    { cdSkills = [#willpower, #intellect, #combat]
    , cdCardTraits = setFromList [Ally, Agency]
    , cdLevel = Just 5
    , cdSlots = [#ally]
    }

studious3 :: CardDef
studious3 =
  permanent
    $ (asset "05276" "Studious" 0 Seeker)
      { cdCardTraits = singleton Talent
      , cdLevel = Just 3
      }

anotherDayAnotherDollar3 :: CardDef
anotherDayAnotherDollar3 =
  permanent
    $ (asset "05278" "Another Day, Another Dollar" 0 Rogue)
      { cdCardTraits = singleton Talent
      , cdLevel = Just 3
      }

dayanaEsperence3 :: CardDef
dayanaEsperence3 =
  (asset "05279" ("Dayana Esperence" <:> "Deals with \"Devils\"") 4 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Ally, Witch]
    , cdLevel = Just 3
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Secret 3
    }

annaKaslow4 :: CardDef
annaKaslow4 =
  (asset "05283" ("Anna Kaslow" <:> "Mysterious Soothsayer") 3 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Ally, Clairvoyant]
    , cdLevel = Just 4
    , cdSlots = [#ally]
    , cdCardInHandEffects = True
    , cdUnique = True
    }

hallowedMirror :: CardDef
hallowedMirror =
  (asset "05313" "Hallowed Mirror" 2 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic, Occult, Blessed]
    , cdSlots = [#accessory]
    , cdBondedWith = [(3, "05314")]
    }

occultLexicon :: CardDef
occultLexicon =
  (asset "05316" "Occult Lexicon" 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome, Occult]
    , cdSlots = [#hand]
    , cdBondedWith = [(3, "05317")]
    }

doubleDouble4 :: CardDef
doubleDouble4 =
  (asset "05320" "Double, Double" 4 Rogue)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Ritual
    , cdExceptional = True
    , cdSlots = [#arcane]
    , cdLevel = Just 4
    }

wither4 :: CardDef
wither4 =
  (asset "05321" "Wither" 2 Mystic)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdLevel = Just 4
    }

sixthSense4 :: CardDef
sixthSense4 =
  (asset "05322" "Sixth Sense" 3 Mystic)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdLevel = Just 4
    }
