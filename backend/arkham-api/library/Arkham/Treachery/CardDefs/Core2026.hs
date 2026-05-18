{- HLINT ignore "Use camelCase" -}
module Arkham.Treachery.CardDefs.Core2026 where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword

inHarmsWay :: CardDef
inHarmsWay =
  (weakness "12003" "In Harm's Way")
    { cdCardTraits = setFromList [Flaw]
    }

breakingPoint :: CardDef
breakingPoint =
  (weakness "12015" "Breaking Point")
    { cdCardTraits = setFromList [Hardship]
    }

pursued :: CardDef
pursued =
  (basicWeakness "12102" "Pursued")
    { cdCardTraits = setFromList [Terror]
    }

syndicateObligations :: CardDef
syndicateObligations =
  (basicWeakness "12103" "Syndicate Obligations")
    { cdCardTraits = setFromList [Pact, Syndicate]
    }

wounded :: CardDef
wounded =
  (basicWeakness "12104" "Wounded")
    { cdCardTraits = setFromList [Injury]
    }

cosmicEvils :: CardDef
cosmicEvils =
  (treachery "12124" "Cosmic Evils" CosmicEvils 3)
    { cdCardTraits = setFromList [Omen]
    , cdKeywords = setFromList [Keyword.Peril]
    }

unspeakableTruths :: CardDef
unspeakableTruths =
  (treachery "12125" "Unspeakable Truths" EldritchLore 2)
    { cdCardTraits = singleton Terror
    }

forbiddenSecrets :: CardDef
forbiddenSecrets =
  (treachery "12126" "Forbidden Secrets" EldritchLore 2)
    { cdCardTraits = singleton Pact
    }

extraplanarVisions :: CardDef
extraplanarVisions =
  (treachery "12127" "Extraplanar Visions" Hallucinations 2)
    { cdCardTraits = singleton Power
    }

wildCompulsion :: CardDef
wildCompulsion =
  (treachery "12128" "Wild Compulsion" Hallucinations 2)
    { cdCardTraits = setFromList [Madness, Bane]
    }

fire1 :: CardDef
fire1 =
  (treachery "12129" "Fire!" Fire1 5)
    { cdCardTraits = singleton Hazard
    }

noxiousSmoke :: CardDef
noxiousSmoke =
  (treachery "12130" "Noxious Smoke" Fire1 2)
    { cdCardTraits = singleton Hazard
    }

mutated1 :: CardDef
mutated1 =
  (treachery "12131" "Mutated!" MadScience 2)
    { cdCardTraits = singleton Mutation
    }

markOfElokoss :: CardDef
markOfElokoss =
  (weakness "12137" "Mark of Elokoss")
    { cdCardTraits = singleton Curse
    , cdEncounterSet = Just SmokeAndMirrors
    , cdEncounterSetQuantity = Just 4
    }

arcaneLock :: CardDef
arcaneLock =
  (treachery "12157" "Arcane Lock" ArcaneLock 2)
    { cdCardTraits = setFromList [Hex, Obstacle]
    }

downpour_BadWeather :: CardDef
downpour_BadWeather =
  (treachery "12158" "Downpour" BadWeather 2)
    { cdCardTraits = singleton Hazard
    }

flashFlood :: CardDef
flashFlood =
  (treachery "12159" "Flash Flood" BadWeather 2)
    { cdCardTraits = singleton Hazard
    }

raisingSuspicions :: CardDef
raisingSuspicions =
  (treachery "12160" "Raising Suspicions" DeadEnds 2)
    { cdCardTraits = singleton Blunder
    }

redHerring :: CardDef
redHerring =
  (treachery "12161" "Red Herring" DeadEnds 2)
    { cdCardTraits = singleton Scheme
    }

aerialPursuit :: CardDef
aerialPursuit =
  surge
    $ (treachery "12163" "Aerial Pursuit" FlyingTerrors 2)
      { cdCardTraits = singleton Scheme
      }

crossfire :: CardDef
crossfire =
  (treachery "12165" "Crossfire" GangsOfArkham 2)
    { cdCardTraits = singleton Hazard
    }

eagerForDeath2 :: CardDef
eagerForDeath2 =
  (treachery "12167" "Eager for Death" Whippoorwills2 2)
    { cdCardTraits = setFromList [Omen]
    }

blasphemousInvocation :: CardDef
blasphemousInvocation =
  (treachery "12190" "Blasphemous Invocation" Cultists 2)
    { cdCardTraits = singleton Hex
    }

unnaturalDecay :: CardDef
unnaturalDecay =
  (treachery "12191" "Unnatural Decay" ReekingDecay 2)
    { cdCardTraits = singleton Curse
    }

putridVapors :: CardDef
putridVapors =
  (treachery "12192" "Putrid Vapors" ReekingDecay 2)
    { cdCardTraits = singleton Hazard
    }

languor :: CardDef
languor =
  (treachery "12193" "Languor" Torment 2)
    { cdCardTraits = setFromList [Hex, Bane]
    }

dissonance :: CardDef
dissonance =
  (treachery "12194" "Dissonance" Torment 2)
    { cdCardTraits = setFromList [Curse, Bane]
    }

torment :: CardDef
torment =
  (peril $ treachery "12195" "Torment" Torment 2)
    { cdCardTraits = setFromList [Power, Bane]
    }

ashenRebirth :: CardDef
ashenRebirth =
  (treachery "12176" "Ashen Rebirth" QueenOfAsh 2)
    { cdCardTraits = singleton Power
    }
