module Arkham.Asset.Cards.ReturnTo where

import Arkham.Asset.Cards.Import
import Arkham.ChaosToken.Types qualified as Token
import Arkham.Keyword qualified as Keyword

physicalTraining2 :: CardDef
physicalTraining2 =
  (asset "50001" "Physical Training" 0 Guardian)
    { cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 2
    }

hyperawareness2 :: CardDef
hyperawareness2 =
  (asset "50003" "Hyperawareness" 0 Seeker)
    { cdSkills = [#intellect, #intellect, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 2
    }

hardKnocks2 :: CardDef
hardKnocks2 =
  (asset "50005" "Hard Knocks" 0 Rogue)
    { cdSkills = [#combat, #combat, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 2
    }

arcaneStudies2 :: CardDef
arcaneStudies2 =
  (asset "50007" "Arcane Studies" 0 Mystic)
    { cdSkills = [#willpower, #willpower, #intellect, #intellect]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 2
    }

digDeep2 :: CardDef
digDeep2 =
  (asset "50009" "Dig Deep" 0 Survivor)
    { cdSkills = [#willpower, #willpower, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = Just 2
    }

rabbitsFoot3 :: CardDef
rabbitsFoot3 =
  (asset "50010" "Rabbit's Foot" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Charm]
    , cdLevel = Just 3
    , cdSlots = [#accessory]
    }

bandolier2 :: CardDef
bandolier2 =
  (asset "51001" "Bandolier" 2 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item]
    , cdSlots = [#body]
    , cdLevel = Just 2
    }

blackjack2 :: CardDef
blackjack2 =
  (asset "51002" "Blackjack" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

strangeSolutionEmpoweringElixir4 :: CardDef
strangeSolutionEmpoweringElixir4 =
  (asset "51004" ("Strange Solution" <:> "Empowering Elixir") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#intellect, #intellect]
    , cdLevel = Just 4
    , cdUses = uses Supply 3
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheSolution
    }

riteOfSeeking2 :: CardDef
riteOfSeeking2 =
  (asset "51007" "Rite of Seeking" 4 Mystic)
    { cdCardTraits = singleton Spell
    , cdSkills = [#intellect]
    , cdLevel = Just 2
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdAlternateCardCodes = ["01689"]
    }

clarityOfMind3 :: CardDef
clarityOfMind3 =
  (asset "51008" "Clarity of Mind" 2 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    , cdLevel = Just 3
    }

thirtyTwoColt2 :: CardDef
thirtyTwoColt2 =
  (asset "52001" ".32 Colt" 2 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUses = uses Ammo 6
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

archaicGlyphsMarkingsOfIsis3 :: CardDef
archaicGlyphsMarkingsOfIsis3 =
  (asset "52004" ("Archaic Glyphs" <:> "Markings of Isis") 2 Seeker)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = Just 3
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheGlyphs
    }

stealth3 :: CardDef
stealth3 =
  (asset "52005" "Stealth" 2 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Talent
    , cdLevel = Just 3
    }

suggestion1 :: CardDef
suggestion1 =
  (asset "52006" "Suggestion" 3 Rogue)
    { cdCardTraits = singleton Spell
    , cdSkills = [#willpower]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = Just 1
    }

alchemicalTransmutation2 :: CardDef
alchemicalTransmutation2 =
  (asset "52007" "Alchemical Transmutation" 0 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    , cdLevel = Just 2
    }

lantern2 :: CardDef
lantern2 =
  (asset "52009" "Lantern" 1 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

gravediggersShovel2 :: CardDef
gravediggersShovel2 =
  (asset "52010" "Gravedigger's Shovel" 1 Survivor)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

survivalKnife2 :: CardDef
survivalKnife2 =
  (asset "53002" "Survival Knife" 2 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

ancientStoneTransientThoughts4 :: CardDef
ancientStoneTransientThoughts4 =
  (asset "53004" ("Ancient Stone" <:> "Transient Thoughts") 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#agility, #agility]
    , cdSlots = [#hand]
    , cdUses = Uses Secret (RecordedCount YouHaveIdentifiedTheStone)
    , cdKeywords = setFromList [Keyword.Researched YouHaveIdentifiedTheStone]
    , cdLevel = Just 4
    }

decoratedSkull3 :: CardDef
decoratedSkull3 =
  (asset "53005" ("Decorated Skull" <:> "Doom Begets Doom") 0 Rogue)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#accessory]
    , cdUses = uses Charge 0
    , cdLevel = Just 3
    }

coltVestPocket2 :: CardDef
coltVestPocket2 =
  (asset "53006" "Colt Vest Pocket" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 5
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

mistsOfRlyeh2 :: CardDef
mistsOfRlyeh2 =
  (asset "53007" "Mists of R'lyeh" 2 Mystic)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 5
    , cdLevel = Just 2
    }

theChthonianStone3 :: CardDef
theChthonianStone3 =
  (asset "53008" ("The Chthonian Stone" <:> "Stygian Waymark") 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdLevel = Just 3
    , cdUses = uses Charge 3
    , cdKeywords =
        singleton
          $ seal
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [Token.Skull, Token.Cultist, Token.Tablet, Token.ElderThing]
    }

onYourOwn3_Exceptional :: CardDef
onYourOwn3_Exceptional =
  permanent
    $ (asset "53010" "On Your Own" 0 Survivor)
      { cdCardTraits = singleton Talent
      , cdLimits = [LimitPerInvestigator 1]
      , cdExceptional = True
      , cdLevel = Just 3
      }

backpack2 :: CardDef
backpack2 =
  (asset "53011" "Backpack" 1 Neutral)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Item
    , cdSlots = [#body]
    , cdLevel = Just 2
    }

dendromorphosis :: CardDef
dendromorphosis =
  (basicWeakness "53012" ("Dendromorphosis" <:> "\"Natural\" Transformation"))
    { cdSlots = [#hand, #hand]
    , cdCardTraits = setFromList [Curse, Flora]
    , cdCost = Nothing
    }

theStarXvii3 :: CardDef
theStarXvii3 =
  (asset "54001" ("The Star • XVII" <:> "You Have Been Chosen") 3 Guardian)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 3
    , cdCardInHandEffects = True
    }

hallowedMirror3 :: CardDef
hallowedMirror3 =
  (asset "54002" "Hallowed Mirror" 2 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Item, Relic, Occult, Blessed]
    , cdSlots = [#accessory]
    , cdLevel = Just 3
    , cdBondedWith = [(3, "05314")]
    }

theWorldXxi3 :: CardDef
theWorldXxi3 =
  (asset "54003" ("The World • XXI" <:> "The Journey is Complete") 3 Seeker)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 3
    , cdCardInHandEffects = True
    }

occultLexicon3 :: CardDef
occultLexicon3 =
  (asset "54004" "Occult Lexicon" 2 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tome, Occult]
    , cdSlots = [#hand]
    , cdLevel = Just 3
    , cdBondedWith = [(3, "05317")]
    }

knightOfSwords3 :: CardDef
knightOfSwords3 =
  (asset "54005" ("Knight of Swords" <:> "Charge Ever Onward") 3 Rogue)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 3
    , cdCardInHandEffects = True
    }

wellConnected3 :: CardDef
wellConnected3 =
  (asset "54006" "Well Connected" 2 Rogue)
    { cdCardTraits = singleton Condition
    , cdSkills = [#intellect, #agility]
    , cdLimits = [LimitPerInvestigator 1]
    , cdLevel = Just 3
    }

theHierophantV3 :: CardDef
theHierophantV3 =
  (asset "54007" ("The Hierophant • V" <:> "Your True Master Awaits") 3 Mystic)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 3
    , cdCardInHandEffects = True
    }

signMagick3 :: CardDef
signMagick3 =
  fast
    (asset "54008" "Sign Magick" 3 Mystic)
      { cdSkills = [#willpower, #intellect]
      , cdCardTraits = setFromList [Ritual, Talent]
      , cdSlots = [#hand]
      , cdLevel = Just 3
      }

nineOfRods3 :: CardDef
nineOfRods3 =
  (asset "54009" ("Nine of Rods" <:> "Every Trial a Lesson") 3 Survivor)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 3
    , cdCardInHandEffects = True
    }

theFool03 :: CardDef
theFool03 =
  (asset "54011" ("The Fool • 0" <:> "Unlimited Potential") 3 Neutral)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = Just 3
    , cdCardInHandEffects = True
    }

moonPendant2 :: CardDef
moonPendant2 =
  (asset "54012" "Moon Pendant" 2 Neutral)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdLevel = Just 2
    , cdCardInHandEffects = True
    }

observed4 :: CardDef
observed4 =
  permanent
    $ (asset "54013" "Observed" 0 Neutral)
      { cdCardTraits = singleton Blessed
      , cdLevel = Just 4
      , cdLimits = [LimitPerInvestigator 1]
      }

theDevilXv :: CardDef
theDevilXv =
  (basicWeakness "54015" ("The Devil • XV" <:> "Your Shadow Hungers"))
    { cdCardTraits = setFromList [Omen, Tarot]
    , cdSlots = [#tarot]
    , cdCardInHandEffects = True
    , cdCanReplace = False
    , cdRevelation = NoRevelation
    , cdCost = Just (StaticCost 3)
    }
