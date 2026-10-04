module Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries where

import Arkham.Homebrew.AgainstTheWendigo.Sets qualified as Set
import Arkham.Homebrew.AgainstTheWendigo.Traits
import Arkham.Treachery.CardDefs.Import

{- | The scenario's own weakness, set aside at setup and dealt out by the "no
resolution" ending. Four copies travel with the encounter set but none of them
are shuffled into the encounter deck.
-}
oldInjury :: CardDef
oldInjury =
  (weakness ":against-the-wendigo:034" "Old Injury")
    { cdCardTraits = setFromList [Injury]
    , cdEncounterSet = Just Set.HanninahValley
    , cdEncounterSetQuantity = Just 4
    }

icyAura :: CardDef
icyAura =
  (treachery ":against-the-wendigo:035" "Icy Aura" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Terror]
    }

mistInTheValley :: CardDef
mistInTheValley =
  (treachery ":against-the-wendigo:036" "Mist in the Valley" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Hazard]
    }

suddenFlood :: CardDef
suddenFlood =
  (treachery ":against-the-wendigo:038" "Sudden Flood" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Desperate, River]
    }

ambush :: CardDef
ambush =
  surge
    $ (treachery ":against-the-wendigo:039" "Ambush" Set.HanninahValley 2)
      { cdCardTraits = setFromList [Trap]
      }

campfire :: CardDef
campfire =
  (treachery ":against-the-wendigo:041" "Campfire" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Hazard]
    }

winterSettles :: CardDef
winterSettles =
  peril
    $ (treachery ":against-the-wendigo:044" "Winter Settles" Set.HanninahValley 2)
      { cdCardTraits = setFromList [Hazard]
      }

unexpectedObstacles :: CardDef
unexpectedObstacles =
  (treachery ":against-the-wendigo:046" "Unexpected Obstacles" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Obstacle]
    }

rapid :: CardDef
rapid =
  (treachery ":against-the-wendigo:049" "Rapid" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Hazard, River]
    }

skinwalker :: CardDef
skinwalker =
  (treachery ":against-the-wendigo:050" "Skinwalker" Set.HanninahValley 1)
    { cdCardTraits = setFromList [Scheme, Indigenous, Wild]
    }

trapper :: CardDef
trapper =
  (treachery ":against-the-wendigo:051" "Trapper" Set.HanninahValley 2)
    { cdCardTraits = setFromList [Scheme]
    }

-- The Wendigo's Myth set, shuffled in when agenda 1 advances.

blazingAttack :: CardDef
blazingAttack =
  (treachery ":against-the-wendigo:052" "Blazing Attack" Set.WendigosMyth 1)
    { cdCardTraits = setFromList [Hazard, Terror, Wendigo]
    }

coldSpirit :: CardDef
coldSpirit =
  (treachery ":against-the-wendigo:053" "Cold Spirit" Set.WendigosMyth 1)
    { cdCardTraits = setFromList [Omen, Terror]
    }

somethingIsStalkingYou :: CardDef
somethingIsStalkingYou =
  (treachery ":against-the-wendigo:055" "Something Is Stalking You" Set.WendigosMyth 1)
    { cdCardTraits = setFromList [Omen, Terror, Wendigo]
    }

terrifyingVisions :: CardDef
terrifyingVisions =
  (treachery ":against-the-wendigo:056" "Terrifying Visions" Set.WendigosMyth 1)
    { cdCardTraits = setFromList [Madness, Wendigo]
    }

-- Story-card backs.

-- | The back of Bernard's Fate (v. II).
theClearingOfTheSacrifices :: CardDef
theClearingOfTheSacrifices =
  ( treachery
      ":against-the-wendigo:025b"
      "The Clearing of the Sacrifices"
      Set.HanninahValley
      1
  )
    { cdCardTraits = setFromList [Terror]
    , cdOtherSide = Just ":against-the-wendigo:025"
    }

-- | The back of Norman's Fate (v. I).
manEaters :: CardDef
manEaters =
  (treachery ":against-the-wendigo:026b" "Man-eaters" Set.HanninahValley 1)
    { cdCardTraits = setFromList [Obstacle, Terror, Wendigo]
    , cdOtherSide = Just ":against-the-wendigo:026"
    }
