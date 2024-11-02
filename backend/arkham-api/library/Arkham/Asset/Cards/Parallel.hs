module Arkham.Asset.Cards.Parallel where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

daisysToteBagAdvanced :: CardDef
daisysToteBagAdvanced =
  signature "01002"
    $ (asset "90002" "Daisy's Tote Bag" 2 Neutral)
      { cdSkills = [#willpower, #intellect, #wild, #wild]
      , cdCardTraits = setFromList [Item]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Advanced]
      }

theNecronomiconAdvanced :: CardDef
theNecronomiconAdvanced =
  (weakness "90003" ("The Necronomicon" <:> "John Dee Translation"))
    { cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdKeywords = setFromList [Keyword.Advanced]
    }

heirloomOfHyperboreaAdvanced :: CardDef
heirloomOfHyperboreaAdvanced =
  signature "90017"
    $ (asset "90018" ("Heirloom of Hyperborea" <:> "Artifact from Another Life") 3 Neutral)
      { cdSkills = [#willpower, #combat, #wild, #wild]
      , cdCardTraits = setFromList [Item, Relic]
      , cdUnique = True
      , cdSlots = [#accessory]
      , cdKeywords = setFromList [Keyword.Advanced]
      }

directiveDueDiligence :: CardDef
directiveDueDiligence =
  signature "90024"
    $ permanent
    $ asset "90025" ("Directive" <:> "Due Diligence") 0 Neutral

directiveRedTape :: CardDef
directiveRedTape =
  signature "90024"
    $ permanent
    $ asset "90026" ("Directive" <:> "Red Tape") 0 Neutral

directiveConsultExperts :: CardDef
directiveConsultExperts =
  signature "90024"
    $ permanent
    $ asset "90027" ("Directive" <:> "Consult Experts") 0 Neutral

directiveSeekTheTruth :: CardDef
directiveSeekTheTruth =
  signature "90024"
    $ permanent
    $ asset "90028" ("Directive" <:> "Seek the Truth") 0 Neutral

directiveLeaveNoDoubt :: CardDef
directiveLeaveNoDoubt =
  signature "90024"
    $ permanent
    $ asset "90029" ("Directive" <:> "Leave No Doubt") 0 Neutral

rolands38SpecialAdvanced :: CardDef
rolands38SpecialAdvanced =
  signature "90024"
    $ (asset "90030" "Roland's .38 Special" 3 Neutral)
      { cdSkills = [#intellect, #combat, #agility, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdUnique = True
      , cdUses = uses Ammo 4
      , cdSlots = [#hand]
      , cdKeywords = singleton Keyword.Advanced
      }

tidalMemento :: CardDef
tidalMemento =
  signature "90037"
    $ permanent
    $ (asset "90038" "Tidal Memento" 0 Neutral)
      { cdCardTraits = setFromList [Item, Charm]
      , cdUnique = True
      }

wendysAmuletAdvanced :: CardDef
wendysAmuletAdvanced =
  signature "90037"
    $ (asset "90039" "Wendy's Amulet" 2 Neutral)
      { cdSkills = [#willpower, #wild, #wild]
      , cdCardTraits = setFromList [Item, Relic]
      , cdUnique = True
      , cdSlots = [#accessory]
      , cdKeywords = singleton Keyword.Advanced
      }

petesGuitar :: CardDef
petesGuitar =
  signature "90046"
    $ (asset "90047" ("Pete's Guitar" <:> "Still Holdin' Up") 2 Neutral)
      { cdCardTraits = setFromList [Item, Instrument]
      , cdKeywords = singleton Keyword.Replacement
      }

jimsTrumpetAdvanced :: CardDef
jimsTrumpetAdvanced =
  signature "90049"
    $ (asset "90050" ("Jim's Trumpet" <:> "The Dead Speak") 2 Neutral)
      { cdSkills = [#willpower, #willpower, #wild, #wild]
      , cdCardTraits = setFromList [Item, Instrument, Relic]
      , cdUnique = True
      , cdSlots = [#hand]
      , cdKeywords = singleton Keyword.Advanced
      }

theBeyondBleakNetherworld :: CardDef
theBeyondBleakNetherworld =
  signature "90049"
    $ permanent
    $ (asset "90052" ("The Beyond" <:> "Bleak Netherworld") 0 Neutral)
      { cdCardTraits = setFromList [Sanctum, Spectral]
      }

zoeysCrossAdvanced :: CardDef
zoeysCrossAdvanced =
  signature "90059"
    $ (asset "90060" ("Zoey's Cross" <:> "Symbol of Conviction") 1 Neutral)
      { cdSkills = [#combat, #combat, #wild]
      , cdCardTraits = setFromList [Item, Charm]
      , cdUnique = True
      , cdSlots = [#accessory]
      , cdKeywords = singleton Keyword.Advanced
      }

trustyBullwhipAdvanced :: CardDef
trustyBullwhipAdvanced =
  signature "90062"
    . fast
    $ (asset "90063" "Trusty Bullwhip" 1 Neutral)
      { cdCardTraits = setFromList [Item, Weapon, Melee]
      , cdSkills = [#intellect, #agility, #agility, #wild]
      , cdSlots = [#hand]
      }

theCodexOfAgesAdvanced :: CardDef
theCodexOfAgesAdvanced =
  signature "90081"
    $ (asset "90082" ("The Codex of Ages" <:> "finis omnium nunc est") 2 Neutral)
      { cdSkills = [#willpower, #intellect, #wild]
      , cdCardTraits = setFromList [Item, Relic, Tome, Blessed]
      , cdUnique = True
      , cdSlots = [#hand]
      , cdKeywords =
          setFromList
            [ Keyword.Advanced
            , Keyword.Seal (Keyword.SealOneOf $ Keyword.Sealing #eldersign :| [Keyword.SealUpTo 3 #bless])
            ]
      }

jennysTwin45sAdvanced :: CardDef
jennysTwin45sAdvanced =
  signature "90084"
    $ (asset "90085" ("Jenny's Twin .45s" <:> "A Perfect Fit") 0 Neutral)
      { cdSkills = [#agility, #wild, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdCost = Just DynamicCost
      , cdUnique = True
      , cdSlots = [#hand, #hand]
      , cdUses = uses Ammo 0
      , cdKeywords = singleton Keyword.Advanced
      }
