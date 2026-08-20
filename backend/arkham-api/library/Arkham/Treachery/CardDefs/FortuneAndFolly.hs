module Arkham.Treachery.CardDefs.FortuneAndFolly where

import Arkham.Treachery.CardDefs.Import

arcaneSpotlightA :: CardDef
arcaneSpotlightA =
  (treachery "88039a" "Arcane Spotlight" FortuneAndFolly 1)
    { cdCardTraits = setFromList [Hex, Obstacle]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "queen")]
    }

arcaneSpotlightB :: CardDef
arcaneSpotlightB =
  (treachery "88039b" "Arcane Spotlight" FortuneAndFolly 1)
    { cdCardTraits = setFromList [Hex, Obstacle]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "jack")]
    }

arcaneSpotlightC :: CardDef
arcaneSpotlightC =
  (treachery "88039c" "Arcane Spotlight" FortuneAndFolly 1)
    { cdCardTraits = setFromList [Hex, Obstacle]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "ten")]
    }

avariceCallsA :: CardDef
avariceCallsA =
  peril
    $ (treachery "88040a" "Avarice Calls" FortuneAndFolly 1)
      { cdCardTraits = singleton Curse
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "queen")]
      }

avariceCallsB :: CardDef
avariceCallsB =
  peril
    $ (treachery "88040b" "Avarice Calls" FortuneAndFolly 1)
      { cdCardTraits = singleton Curse
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "jack")]
      }

avariceCallsC :: CardDef
avariceCallsC =
  peril
    $ (treachery "88040c" "Avarice Calls" FortuneAndFolly 1)
      { cdCardTraits = singleton Curse
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "ten")]
      }

dimensionalHypnosisA :: CardDef
dimensionalHypnosisA =
  (treachery "88051a" "Dimensional Hypnosis" PlanInShambles 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "five")]
    }

dimensionalHypnosisB :: CardDef
dimensionalHypnosisB =
  (treachery "88051b" "Dimensional Hypnosis" PlanInShambles 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "five")]
    }

dimensionalHypnosisC :: CardDef
dimensionalHypnosisC =
  (treachery "88051c" "Dimensional Hypnosis" PlanInShambles 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "four")]
    }

gripOfTheBeyondA :: CardDef
gripOfTheBeyondA =
  (treachery "88052a" "Grip of the Beyond" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "five")]
    }

gripOfTheBeyondB :: CardDef
gripOfTheBeyondB =
  (treachery "88052b" "Grip of the Beyond" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "five")]
    }

gripOfTheBeyondC :: CardDef
gripOfTheBeyondC =
  (treachery "88052c" "Grip of the Beyond" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "four")]
    }

huntersHungerA :: CardDef
huntersHungerA =
  (treachery "88053a" "Hunter's Hunger" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "four")]
    }

huntersHungerB :: CardDef
huntersHungerB =
  (treachery "88053b" "Hunter's Hunger" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "four")]
    }

inconvenientQuesitoningA :: CardDef
inconvenientQuesitoningA =
  (treachery "88038a" "Inconvenient Questioning" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "nine")]
    }

inconvenientQuesitoningB :: CardDef
inconvenientQuesitoningB =
  (treachery "88038b" "Inconvenient Questioning" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "nine")]
    }

inconvenientQuesitoningC :: CardDef
inconvenientQuesitoningC =
  (treachery "88038c" "Inconvenient Questioning" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "eight")]
    }

inconvenientQuesitoningD :: CardDef
inconvenientQuesitoningD =
  (treachery "88038d" "Inconvenient Questioning" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "eight")]
    }

obsessedGamblerA :: CardDef
obsessedGamblerA =
  (treachery "88042a" "Obsessed Gambler" FortuneAndFolly 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "eight")]
    }

obsessedGamblerB :: CardDef
obsessedGamblerB =
  (treachery "88042b" "Obsessed Gambler" FortuneAndFolly 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "seven")]
    }

obsessedGamblerC :: CardDef
obsessedGamblerC =
  (treachery "88042c" "Obsessed Gambler" FortuneAndFolly 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "seven")]
    }

suspiciousGazeA :: CardDef
suspiciousGazeA =
  (treachery "88041a" "Suspicious Gaze" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "eight")]
    }

suspiciousGazeB :: CardDef
suspiciousGazeB =
  (treachery "88041b" "Suspicious Gaze" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "seven")]
    }

suspiciousGazeC :: CardDef
suspiciousGazeC =
  (treachery "88041c" "Suspicious Gaze" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "seven")]
    }
