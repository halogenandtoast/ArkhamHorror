{- | The Symphony of Erich Zann's assets.

The four instrument rewards and Auguste Gaudin are printed on *player* backs:
each can be earned into an investigator's deck when its Musician is saved. The
Piano keeps the encounter back -- it is attached to a Backstage Room by act 2
and never enters a deck, it only flips to its story side.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.TheSymphonyOfErichZann.Sets qualified as Set
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits hiding (pattern Piano)

yinsDrumsticks :: CardDef
yinsDrumsticks =
  (storyAsset ":the-symphony-of-erich-zann:024" "Yin's Drumsticks" 1 Set.TheSymphonyOfErichZann)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat, #wild]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdOutOfPlayEffects = [CommittedEffect]
    }

pagesViolin :: CardDef
pagesViolin =
  (storyAsset ":the-symphony-of-erich-zann:025" "Page's Violin" 3 Set.TheSymphonyOfErichZann)
    { cdCardTraits = setFromList [Item, Instrument]
    , cdSkills = [#wild]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

laFrattasPianoKey :: CardDef
laFrattasPianoKey =
  (storyAsset ":the-symphony-of-erich-zann:026" "La Fratta's Piano Key" 2 Set.TheSymphonyOfErichZann)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSkills = [#agility, #agility]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

walkersTrumpet :: CardDef
walkersTrumpet =
  (storyAsset ":the-symphony-of-erich-zann:027" "Walker's Trumpet" 2 Set.TheSymphonyOfErichZann)
    { cdCardTraits = setFromList [Item, Instrument]
    , cdSkills = [#willpower, #willpower]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

augusteGaudinMaestroOfSymphonies :: CardDef
augusteGaudinMaestroOfSymphonies =
  ( storyAsset
      ":the-symphony-of-erich-zann:044"
      ("Auguste Gaudin" <:> "Maestro of Symphonies")
      2
      Set.TheSymphonyOfErichZann
  )
    { cdCardTraits = setFromList [Ally, Musician]
    , cdSkills = [#willpower, #intellect]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

{- | Attached to a Backstage Room by act 2 when Isabel La Fratta is being played
as an investigator, in place of the Isabel La Fratta enemy.
-}
thePiano :: CardDef
thePiano =
  (encounterAsset_ ":the-symphony-of-erich-zann:045" "The Piano" Set.TheSymphonyOfErichZann)
    { cdCardTraits = singleton Instrument
    , cdUnique = True
    , cdVictoryPoints = Just 1
    , cdOtherSide = Just ":the-symphony-of-erich-zann:045b"
    }
