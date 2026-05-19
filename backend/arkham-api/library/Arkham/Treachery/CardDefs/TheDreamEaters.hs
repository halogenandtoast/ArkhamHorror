module Arkham.Treachery.CardDefs.TheDreamEaters where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword

rookieMistake :: CardDef
rookieMistake =
  (weakness "06007" "Rookie Mistake")
    { cdCardTraits = setFromList [Blunder, Flaw]
    }

shockingDiscovery :: CardDef
shockingDiscovery =
  (weakness "06009" "Shocking Discovery")
    { cdCardTraits = setFromList [Blunder, Mystery]
    , cdOutOfPlayEffects = [InSearchEffect]
    }

detachedFromReality :: CardDef
detachedFromReality =
  (weakness "06014" "Detached from Reality")
    { cdCardTraits = setFromList [Madness]
    }

bloodlust :: CardDef
bloodlust =
  (weakness "06019" "Bloodlust")
    { cdCardTraits = setFromList [Madness]
    , cdKeywords = singleton (Keyword.Bonded 3 "06018")
    }

selfCentered :: CardDef
selfCentered =
  (basicWeakness "06035" "Self-Centered")
    { cdCardTraits = setFromList [Flaw]
    , cdDeckRestrictions = [MultiplayerOnly]
    }

narcolepsy :: CardDef
narcolepsy =
  (basicWeakness "06037" "Narcolepsy")
    { cdCardTraits = setFromList [Madness]
    , cdDeckRestrictions = [MultiplayerOnly]
    }

lostInTheWoods :: CardDef
lostInTheWoods =
  (treachery "06062" "Lost in the Woods" BeyondTheGatesOfSleep 2)
    { cdCardTraits = singleton Blunder
    , cdKeywords = singleton Keyword.Surge
    }

outbreak :: CardDef
outbreak =
  (treachery "06083" "Outbreak" WakingNightmare 3)
    { cdCardTraits = singleton Hazard
    }

willOfTheSpiderMother :: CardDef
willOfTheSpiderMother =
  (treachery "06085" "Will of the Spider-Mother" AgentsOfAtlachNacha 2)
    { cdCardTraits = singleton Power
    }

lawOfYgirothChaos :: CardDef
lawOfYgirothChaos =
  (treachery "06087" ("Law of 'Ygiroth" <:> "Chaos") AgentsOfNyarlathotep 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

lawOfYgirothDiscord :: CardDef
lawOfYgirothDiscord =
  (treachery "06088" ("Law of 'Ygiroth" <:> "Discord") AgentsOfNyarlathotep 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

lawOfYgirothPandemonium :: CardDef
lawOfYgirothPandemonium =
  (treachery "06089" ("Law of 'Ygiroth" <:> "Pandemonium") AgentsOfNyarlathotep 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

whispersOfHypnos :: CardDef
whispersOfHypnos =
  (treachery "06090" "Whispers of Hypnos" WhispersOfHypnos 3)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

dreamersCurse :: CardDef
dreamersCurse =
  (treachery "06093" "Dreamer's Curse" DreamersCurse 2)
    { cdCardTraits = singleton Curse
    }

somniphobia :: CardDef
somniphobia =
  (treachery "06094" "Somniphobia" DreamersCurse 2)
    { cdCardTraits = singleton Terror
    }

deeperSlumber :: CardDef
deeperSlumber =
  (treachery "06095" "Deeper Slumber" DreamersCurse 2)
    { cdCardTraits = singleton Curse
    }

dreamlandsEclipse :: CardDef
dreamlandsEclipse =
  (treachery "06096" "Dreamlands Eclipse" Dreamlands 2)
    { cdCardTraits = singleton Power
    }

prismaticPhenomenon :: CardDef
prismaticPhenomenon =
  (treachery "06097" "Prismatic Phenomenon" Dreamlands 2)
    { cdCardTraits = singleton Power
    }

nightTerrors :: CardDef
nightTerrors =
  (treachery "06098" "Night Terrors" MergingRealities 2)
    { cdCardTraits = singleton Terror
    }

glimpseOfTheUnderworld :: CardDef
glimpseOfTheUnderworld =
  (treachery "06099" "Glimpse of the Underworld" MergingRealities 2)
    { cdCardTraits = singleton Terror
    }

threadsOfReality :: CardDef
threadsOfReality =
  (treachery "06100" "Threads of Reality" MergingRealities 2)
    { cdCardTraits = singleton Power
    }

sickeningWebs :: CardDef
sickeningWebs =
  (treachery "06103" "Sickening Webs" Spiders 2)
    { cdCardTraits = singleton Obstacle
    }

huntedByCorsairs :: CardDef
huntedByCorsairs =
  (treachery "06104" "Hunted by Corsairs" Corsairs 2)
    { cdCardTraits = singleton Scheme
    }

zoogBurrow :: CardDef
zoogBurrow =
  (treachery "06109" "Zoog Burrow" Zoogs 1)
    { cdCardTraits = singleton Hazard
    }

songOfTheMagahBird :: CardDef
songOfTheMagahBird =
  (treachery "06153" "Song of the Magah Bird" TheSearchForKadath 2)
    { cdCardTraits = singleton Curse
    }

wondrousLands :: CardDef
wondrousLands =
  (treachery "06154" "Wondrous Lands" TheSearchForKadath 2)
    { cdCardTraits = singleton Power
    }

endlessDescent :: CardDef
endlessDescent =
  (treachery "06190" "Endless Descent" AThousandShapesOfHorror 4)
    { cdCardTraits = singleton Curse
    }

indescribableApparition :: CardDef
indescribableApparition =
  (treachery "06191" "Indescribable Apparition" AThousandShapesOfHorror 2)
    { cdCardTraits = singleton Curse
    }

glowingEyes :: CardDef
glowingEyes =
  (treachery "06192" "Glowing Eyes" AThousandShapesOfHorror 2)
    { cdCardTraits = singleton Terror
    }

deceptiveMemories :: CardDef
deceptiveMemories =
  (treachery "06193" "Deceptive Memories" AThousandShapesOfHorror 2)
    { cdCardTraits = singleton Terror
    }

secretsInTheAttic :: CardDef
secretsInTheAttic =
  (treachery "06194" "Secrets in the Attic" AThousandShapesOfHorror 2)
    { cdCardTraits = singleton Scheme
    }

closeWatch :: CardDef
closeWatch =
  (treachery "06230" "Close Watch" DarkSideOfTheMoon 3)
    { cdCardTraits = singleton Scheme
    }

forcedIntoHiding :: CardDef
forcedIntoHiding =
  (treachery "06231" "Forced into Hiding" DarkSideOfTheMoon 3)
    { cdCardTraits = singleton Terror
    }

lunarPatrol :: CardDef
lunarPatrol =
  (treachery "06232" "Lunar Patrol" DarkSideOfTheMoon 2)
    { cdCardTraits = singleton Scheme
    }

falseAwakening :: CardDef
falseAwakening =
  (weakness "06233" "False Awakening")
    { cdCardTraits = setFromList [Curse]
    , cdEncounterSet = Just DarkSideOfTheMoon
    , cdEncounterSetQuantity = Just 1
    }

tasteOfLifeblood :: CardDef
tasteOfLifeblood =
  (treachery "06268" "Taste of Lifeblood" PointOfNoReturn 2)
    { cdCardTraits = singleton Hazard
    }

litByDeathFire :: CardDef
litByDeathFire =
  (treachery "06269" "Lit by Death-Fire" PointOfNoReturn 2)
    { cdCardTraits = singleton Hazard
    }

unexpectedAmbush :: CardDef
unexpectedAmbush =
  (treachery "06270" "Unexpected Ambush" PointOfNoReturn 2)
    { cdCardTraits = singleton Scheme
    }

dholeTunnel :: CardDef
dholeTunnel =
  (treachery "06272" "Dhole Tunnel" TerrorOfTheVale 3)
    { cdCardTraits = singleton Hazard
    }

shadowOfAtlachNacha :: CardDef
shadowOfAtlachNacha =
  (treachery "06274" "Shadow of Atlach-Nacha" DescentIntoThePitch 2)
    { cdCardTraits = singleton Curse
    }

falseAwakeningPointOfNoReturn :: CardDef
falseAwakeningPointOfNoReturn =
  (weakness "06275" "False Awakening")
    { cdCardTraits = setFromList [Curse]
    , cdEncounterSet = Just PointOfNoReturn
    , cdEncounterSetQuantity = Just 1
    }

whisperingChaosNorth :: CardDef
whisperingChaosNorth =
  (treachery "06314" ("Whispering Chaos" <:> "North") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Power
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

whisperingChaosEast :: CardDef
whisperingChaosEast =
  (treachery "06315" ("Whispering Chaos" <:> "East") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Power
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

whisperingChaosSouth :: CardDef
whisperingChaosSouth =
  (treachery "06316" ("Whispering Chaos" <:> "South") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Power
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

whisperingChaosWest :: CardDef
whisperingChaosWest =
  (treachery "06317" ("Whispering Chaos" <:> "West") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Power
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

myriadForms :: CardDef
myriadForms =
  (treachery "06318" "Myriad Forms" WhereTheGodsDwell 2)
    { cdCardTraits = singleton Power
    }

restlessJourneyFallacy :: CardDef
restlessJourneyFallacy =
  (treachery "06319" ("Restless Journey" <:> "Fallacy") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Curse
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

restlessJourneyHardship :: CardDef
restlessJourneyHardship =
  (treachery "06320" ("Restless Journey" <:> "Hardship") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Curse
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

restlessJourneyLies :: CardDef
restlessJourneyLies =
  (treachery "06321" ("Restless Journey" <:> "Lies") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Curse
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

abandonedByTheGods :: CardDef
abandonedByTheGods =
  (treachery "06322" "Abandoned by the Gods" WhereTheGodsDwell 2)
    { cdCardTraits = singleton Curse
    , cdKeywords = setFromList [Keyword.Peril]
    }

theSpinnerInDarkness :: CardDef
theSpinnerInDarkness =
  (treachery "06352" "The Spinner in Darkness" WeaverOfTheCosmos 2)
    { cdCardTraits = singleton Power
    }

caughtInAWeb :: CardDef
caughtInAWeb =
  (treachery "06353" "Caught in a Web" WeaverOfTheCosmos 3)
    { cdCardTraits = singleton Hazard
    }

endlessWeaving :: CardDef
endlessWeaving =
  (treachery "06354" "Endless Weaving" WeaverOfTheCosmos 3)
    { cdCardTraits = singleton Scheme
    }
