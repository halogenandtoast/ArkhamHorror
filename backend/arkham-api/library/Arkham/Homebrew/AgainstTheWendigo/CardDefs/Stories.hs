{- | Against the Wendigo's story cards.

Each of the three students is printed twice, with the same front and two
different backs; setup removes one of each pair at random, so which fate a
student met is decided before the game starts. The surviving three are the
Students' Fate deck.

Hanninah's Gold, Charlie Foxtail's Destiny and The Knowledge of the Cold are
the valley's three optional discoveries, each flipping into a card of a
different type.
-}
module Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories where

import Arkham.Card.CardDef
import Arkham.Homebrew.AgainstTheWendigo.Sets qualified as Set
import Arkham.Prelude
import Arkham.Story.CardDefs.Base

hanninahsGold :: CardDef
hanninahsGold =
  doubleSided $ story ":against-the-wendigo:021" "Hanninah's Gold" Set.HanninahValley

charlieFoxtailsDestiny :: CardDef
charlieFoxtailsDestiny =
  doubleSided
    $ story ":against-the-wendigo:022" "Charlie Foxtail's Destiny" Set.HanninahValley

theKnowledgeOfTheCold :: CardDef
theKnowledgeOfTheCold =
  doubleSided
    $ story ":against-the-wendigo:023" "The Knowledge of the Cold" Set.HanninahValley

-- | Flips into Bernard Epstein, who came back as something else.
bernardsFateV1 :: CardDef
bernardsFateV1 =
  doubleSided $ story ":against-the-wendigo:024" "Bernard's Fate (v. I)" Set.HanninahValley

-- | Flips into The Clearing of the Sacrifices; Bernard did not come back at all.
bernardsFateV2 :: CardDef
bernardsFateV2 =
  doubleSided $ story ":against-the-wendigo:025" "Bernard's Fate (v. II)" Set.HanninahValley

-- | Flips into Man-eaters.
normansFateV1 :: CardDef
normansFateV1 =
  doubleSided $ story ":against-the-wendigo:026" "Norman's Fate (v. I)" Set.HanninahValley

-- | Flips into Norman Falkner, the one student who can be brought home alive.
normansFateV2 :: CardDef
normansFateV2 =
  doubleSided $ story ":against-the-wendigo:027" "Norman's Fate (v. II)" Set.HanninahValley

-- | Flips into The Heart of the Forest.
sylviasFateV1 :: CardDef
sylviasFateV1 =
  doubleSided $ story ":against-the-wendigo:028" "Sylvia's Fate (v. I)" Set.HanninahValley

-- | Flips into Sylvia Davidson, possessed.
sylviasFateV2 :: CardDef
sylviasFateV2 =
  doubleSided $ story ":against-the-wendigo:029" "Sylvia's Fate (v. II)" Set.HanninahValley
