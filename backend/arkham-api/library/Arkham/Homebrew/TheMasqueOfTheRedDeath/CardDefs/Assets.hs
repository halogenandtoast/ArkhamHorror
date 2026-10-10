{- | The Masque of the Red Death's assets.

Eight masked [[Guest]] story assets attend the ball; two are removed from the
game at random and the remaining six take a chamber each. Every one of them is a
double-faced encounter asset: the @Guest@ face is only parleyable, and act 1's
advance flips all of them to a @Victim@ face that has health, sanity, Victory 0
and a @Forced@ ability that holds you in the room unless you feed it doom.

Prospero Prince is double-faced too, but across card *types*: his asset face is
the host working the room, and the back (@023b@, in
"Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies") is the Elite enemy he
becomes when the masquerade falls apart.

Plague Polyp is the only card here on a player back: Resolution 2 offers it to
one investigator's deck.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Card.CardCode (CardCode)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Sets qualified as Set
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Traits (pattern Victim)

{- | A masked guest, face up on its @Guest@ side. Parley only: no health, no
sanity, no victory.
-}
guest :: CardCode -> Name -> CardCode -> CardDef
guest cardCode name otherSide =
  (encounterAsset_ cardCode name Set.TheMasqueOfTheRedDeath)
    { cdCardTraits = setFromList [Cultist, Guest]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just otherSide
    }

-- | The same guest once the Red Death has found them.
victim :: CardCode -> Name -> CardCode -> CardDef
victim cardCode name otherSide =
  (encounterAsset_ cardCode name Set.TheMasqueOfTheRedDeath)
    { cdCardTraits = setFromList [Cultist, Victim]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just otherSide
    , cdVictoryPoints = Just 0
    }

theDevilsMorbidlyFascinated :: CardDef
theDevilsMorbidlyFascinated =
  guest
    ":the-masque-of-the-red-death:015"
    ("The Devils" <:> "Morbidly Fascinated")
    ":the-masque-of-the-red-death:015b"

theDevilsMorbidlyFated :: CardDef
theDevilsMorbidlyFated =
  victim
    ":the-masque-of-the-red-death:015b"
    ("The Devils" <:> "Morbidly Fated")
    ":the-masque-of-the-red-death:015"

theFoxLookingForAction :: CardDef
theFoxLookingForAction =
  guest
    ":the-masque-of-the-red-death:016"
    ("The Fox" <:> "Looking for Action")
    ":the-masque-of-the-red-death:016b"

theFoxLookingForRelease :: CardDef
theFoxLookingForRelease =
  victim
    ":the-masque-of-the-red-death:016b"
    ("The Fox" <:> "Looking for Release")
    ":the-masque-of-the-red-death:016"

theMothDrawnToTheFlame :: CardDef
theMothDrawnToTheFlame =
  guest
    ":the-masque-of-the-red-death:017"
    ("The Moth" <:> "Drawn to the Flame")
    ":the-masque-of-the-red-death:017b"

theMothBurnedByTheFlame :: CardDef
theMothBurnedByTheFlame =
  victim
    ":the-masque-of-the-red-death:017b"
    ("The Moth" <:> "Burned by the Flame")
    ":the-masque-of-the-red-death:017"

theOwlReadingIntoYou :: CardDef
theOwlReadingIntoYou =
  guest
    ":the-masque-of-the-red-death:018"
    ("The Owl" <:> "Reading Into You")
    ":the-masque-of-the-red-death:018b"

theOwlReadingHerLast :: CardDef
theOwlReadingHerLast =
  victim
    ":the-masque-of-the-red-death:018b"
    ("The Owl" <:> "Reading Her Last")
    ":the-masque-of-the-red-death:018"

thePeacockCenterOfAttention :: CardDef
thePeacockCenterOfAttention =
  guest
    ":the-masque-of-the-red-death:019"
    ("The Peacock" <:> "Center of Attention")
    ":the-masque-of-the-red-death:019b"

thePeacockCenterOfAffliction :: CardDef
thePeacockCenterOfAffliction =
  victim
    ":the-masque-of-the-red-death:019b"
    ("The Peacock" <:> "Center of Affliction")
    ":the-masque-of-the-red-death:019"

theRavenNotEasilyImpressed :: CardDef
theRavenNotEasilyImpressed =
  guest
    ":the-masque-of-the-red-death:020"
    ("The Raven" <:> "Not Easily Impressed")
    ":the-masque-of-the-red-death:020b"

theRavenNotEasilyDisposed :: CardDef
theRavenNotEasilyDisposed =
  victim
    ":the-masque-of-the-red-death:020b"
    ("The Raven" <:> "Not Easily Disposed")
    ":the-masque-of-the-red-death:020"

theVultureWatchingTheFeast :: CardDef
theVultureWatchingTheFeast =
  guest
    ":the-masque-of-the-red-death:021"
    ("The Vulture" <:> "Watching the Feast")
    ":the-masque-of-the-red-death:021b"

theVulturePartOfTheFeast :: CardDef
theVulturePartOfTheFeast =
  victim
    ":the-masque-of-the-red-death:021b"
    ("The Vulture" <:> "Part of the Feast")
    ":the-masque-of-the-red-death:021"

theWaspViolentlyMotivated :: CardDef
theWaspViolentlyMotivated =
  guest
    ":the-masque-of-the-red-death:022"
    ("The Wasp" <:> "Violently Motivated")
    ":the-masque-of-the-red-death:022b"

theWaspViolentlyEmbraced :: CardDef
theWaspViolentlyEmbraced =
  victim
    ":the-masque-of-the-red-death:022b"
    ("The Wasp" <:> "Violently Embraced")
    ":the-masque-of-the-red-death:022"

{- | The host. Not a [[Guest]] -- he is never one of the six dealt to the
chambers, he starts at Grand Ballroom, and his back is an enemy.
-}
prosperoPrinceGregariousHost :: CardDef
prosperoPrinceGregariousHost =
  ( encounterAsset_
      ":the-masque-of-the-red-death:023"
      ("Prospero Prince" <:> "Gregarious Host")
      Set.TheMasqueOfTheRedDeath
  )
    { cdCardTraits = singleton Cultist
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just ":the-masque-of-the-red-death:023b"
    }

-- | Resolution 2's memento. Set aside at setup, never shuffled in.
plaguePolyp :: CardDef
plaguePolyp =
  ( storyAsset
      ":the-masque-of-the-red-death:055"
      ("Plague Polyp" <:> "In Sickness and In Health")
      2
      Set.TheMasqueOfTheRedDeath
  )
    { cdCardTraits = setFromList [Item, Occult, Cursed]
    , cdSkills = [#combat, #wild]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }
