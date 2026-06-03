module Arkham.Treachery.CardDefs.ThePathToCarcosa where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword
import Arkham.EncounterSet qualified as EncounterSet

shellShock :: CardDef
shellShock =
  (weakness "03008" "Shell Shock") {cdCardTraits = setFromList [Flaw]}

starsOfHyades :: CardDef
starsOfHyades =
  (weakness "03013" "Stars of Hyades") {cdCardTraits = setFromList [Curse]}

angeredSpirits :: CardDef
angeredSpirits =
  (weakness "03015" "Angered Spirits") {cdCardTraits = singleton Task}

crisisOfIdentity :: CardDef
crisisOfIdentity =
  (weakness "03019" "Crisis of Identity") {cdCardTraits = singleton Madness}

overzealous :: CardDef
overzealous =
  (basicWeakness "03040" "Overzealous")
    { cdCardTraits = singleton Flaw
    , cdAlternateCardCodes = ["12100"]
    }

drawingTheSign :: CardDef
drawingTheSign =
  (basicWeakness "03041" "Drawing the Sign")
    { cdCardTraits = setFromList [Pact, Madness]
    }

fineDining :: CardDef
fineDining =
  (treachery "03082" "Fine Dining" TheLastKing 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

toughCrowd :: CardDef
toughCrowd =
  (treachery "03083" "Tough Crowd" TheLastKing 2)
    { cdCardTraits = singleton Hazard
    }

whispersInYourHeadDismay :: CardDef
whispersInYourHeadDismay =
  (treachery "03084a" ("Whispers in Your Head" <:> "Dismay") Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

whispersInYourHeadDread :: CardDef
whispersInYourHeadDread =
  (treachery "03084b" ("Whispers in Your Head" <:> "Dread") Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

whispersInYourHeadAnxiety :: CardDef
whispersInYourHeadAnxiety =
  (treachery "03084c" ("Whispers in Your Head" <:> "Anxiety") Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

whispersInYourHeadDoubt :: CardDef
whispersInYourHeadDoubt =
  (treachery "03084d" ("Whispers in Your Head" <:> "Doubt") Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

descentIntoMadness :: CardDef
descentIntoMadness =
  (treachery "03085" "Descent into Madness" Delusions 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Surge
    }

huntedByByakhee :: CardDef
huntedByByakhee =
  (treachery "03087" "Hunted by Byakhee" EncounterSet.Byakhee 2)
    { cdCardTraits = singleton Pact
    }

blackStarsRise :: CardDef
blackStarsRise =
  (treachery "03090" "Black Stars Rise" EvilPortents 2)
    { cdCardTraits = singleton Omen
    }

spiresOfCarcosa :: CardDef
spiresOfCarcosa =
  (treachery "03091" "Spires of Carcosa" EvilPortents 2)
    { cdCardTraits = singleton Omen
    }

twistedToHisWill :: CardDef
twistedToHisWill =
  (treachery "03092" "Twisted to His Will" EvilPortents 2)
    { cdCardTraits = singleton Pact
    }

spiritsTorment :: CardDef
spiritsTorment =
  (treachery "03094" "Spirit's Torment" Hauntings 2)
    { cdCardTraits = setFromList [Curse, Geist]
    }

danceOfTheYellowKing :: CardDef
danceOfTheYellowKing =
  (treachery "03097" "Dance of the Yellow King" HastursGift 2)
    { cdCardTraits = singleton Pact
    }

theKingsEdict :: CardDef
theKingsEdict =
  (treachery "03100" "The King's Edict" CultOfTheYellowSign 2)
    { cdCardTraits = singleton Pact
    }

oozeAndFilth :: CardDef
oozeAndFilth =
  (treachery "03101" "Ooze and Filth" DecayAndFilth 2)
    { cdCardTraits = singleton Hazard
    }

corrosion :: CardDef
corrosion =
  (treachery "03102" "Corrosion" DecayAndFilth 2)
    { cdCardTraits = singleton Hazard
    }

markedByTheSign :: CardDef
markedByTheSign =
  (treachery "03104" "Marked by the Sign" TheStranger 2)
    { cdCardTraits = singleton Pact
    , cdKeywords = singleton Keyword.Peril
    }

thePaleMaskBeckons :: CardDef
thePaleMaskBeckons =
  (treachery "03105" "The Pale Mask Beckons" TheStranger 1)
    { cdCardTraits = setFromList [Omen, Pact]
    }

ledAstray :: CardDef
ledAstray =
  (treachery "03145" "Led Astray" EchoesOfThePast 3)
    { cdCardTraits = singleton Scheme
    , cdKeywords = singleton Keyword.Peril
    }

theCultsSearch :: CardDef
theCultsSearch =
  (treachery "03146" "The Cult's Search" EchoesOfThePast 2)
    { cdCardTraits = singleton Scheme
    }

straitjacket :: CardDef
straitjacket =
  (treachery "03185" "Straitjacket" TheUnspeakableOath 2)
    { cdCardTraits = setFromList [Item, Clothing]
    }

giftOfMadnessPity :: CardDef
giftOfMadnessPity =
  (treachery "03186" ("Gift of Madness" <:> "Pity") TheUnspeakableOath 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

giftOfMadnessMisery :: CardDef
giftOfMadnessMisery =
  (treachery "03187" ("Gift of Madness" <:> "Misery") TheUnspeakableOath 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

wallsClosingIn :: CardDef
wallsClosingIn =
  (treachery "03188" "Walls Closing In" TheUnspeakableOath 3)
    { cdCardTraits = singleton Terror
    }

twinSuns :: CardDef
twinSuns =
  (treachery "03223" "Twin Suns" APhantomOfTruth 2)
    { cdCardTraits = singleton Omen
    }

deadlyFate :: CardDef
deadlyFate =
  (treachery "03224" "Deadly Fate" APhantomOfTruth 3)
    { cdCardTraits = singleton Omen
    }

torturousChords :: CardDef
torturousChords =
  (treachery "03225" "Torturous Chords" APhantomOfTruth 3)
    { cdCardTraits = setFromList [Hex, Terror]
    }

frozenInFearAPhantomOfTruth :: CardDef
frozenInFearAPhantomOfTruth =
  (treachery "03226" "Frozen in Fear" APhantomOfTruth 2)
    { cdCardTraits = singleton Terror
    }

lostSoul :: CardDef
lostSoul =
  (weakness "03227" "Lost Soul")
    { cdCardTraits = setFromList [Madness, Pact]
    , cdEncounterSet = Just APhantomOfTruth
    , cdEncounterSetQuantity = Just 4
    }

eyesInTheWalls :: CardDef
eyesInTheWalls =
  (treachery "03260" "Eyes in the Walls" ThePallidMask 3)
    { cdCardTraits = singleton Terror
    }

theShadowBehindYou :: CardDef
theShadowBehindYou =
  (treachery "03261" "The Shadow Behind You" ThePallidMask 3)
    { cdCardTraits = singleton Terror
    }

thePitBelow :: CardDef
thePitBelow =
  (treachery "03262" "The Pit Below" ThePallidMask 3)
    { cdCardTraits = singleton Hazard
    }

crashingFloods :: CardDef
crashingFloods =
  (treachery "03302" "Crashing Floods" BlackStarsRise 3)
    { cdCardTraits = singleton Omen
    }

worldsMerge :: CardDef
worldsMerge =
  (treachery "03303" "Worlds Merge" BlackStarsRise 3)
    { cdCardTraits = singleton Omen
    }

dismalCurse :: CardDef
dismalCurse =
  (treachery "03337" "Dismal Curse" DimCarcosa 3)
    { cdCardTraits = setFromList [Curse, Terror]
    }

realmOfMadness :: CardDef
realmOfMadness =
  (treachery "03338" "Realm of Madness" DimCarcosa 2)
    { cdCardTraits = singleton Terror
    }

theFinalAct :: CardDef
theFinalAct =
  (treachery "03339" "The Final Act" DimCarcosa 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Surge]
    }

possessionTraitorous :: CardDef
possessionTraitorous =
  (treachery "03340" ("Possession" <:> "Traitorous") DimCarcosa 1)
    { cdCardTraits = setFromList [Hex, Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    , cdCommitRestrictions = [CommittableTreachery]
    }

possessionTorturous :: CardDef
possessionTorturous =
  (treachery "03341" ("Possession" <:> "Torturous") DimCarcosa 1)
    { cdCardTraits = setFromList [Hex, Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

possessionMurderous :: CardDef
possessionMurderous =
  (treachery "03342" ("Possession" <:> "Murderous") DimCarcosa 1)
    { cdCardTraits = setFromList [Hex, Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }
