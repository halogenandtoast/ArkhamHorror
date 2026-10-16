{- | And Then There Were None's assets.

Ten [[Victim]] story assets -- the guests of Soldier Island -- are dealt two to
a room at setup. They print nothing but their two skill icons, and those icons
are the whole puzzle: act 1b tallies the [[Method]] cards and convicts the
victim whose icons match the fewest of them, flipping that one to its
[[Culprit]] enemy side (the backs live in
"Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies").

Between them the ten victims cover every unordered pair of the five icons, so
exactly one of them has the lowest total and the verdict is never a tie.

Fifteen [[Method]] cards, three of each icon, are the murder weapons; setup
removes five of them face down and the remaining ten go two to a room. The four
copies of Antediluvian Sigils are set aside until act 1b hangs them on the
locations nearest the culprit. Spectral Conduit is the only card here on a
player back: Resolution 2 offers it to one investigator's deck.
-}
module Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Card.CardCode (CardCode, flippedCardCode)
import Arkham.Homebrew.AndThenThereWereNone.Sets qualified as Set
import Arkham.Homebrew.AndThenThereWereNone.Traits (pattern Method, pattern Victim)
import Arkham.SkillType (SkillIcon)

-- | One of the ten guests, face up on their @Victim@ side. No cost, no stats, two icons.
victim :: CardCode -> Name -> [SkillIcon] -> CardDef
victim cardCode name icons =
  (encounterAsset_ cardCode name Set.AndThenThereWereNone)
    { cdCardTraits = singleton Victim
    , cdSkills = icons
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just (flippedCardCode cardCode)
    }

-- | A murder weapon. Facedown beneath a location until someone reveals it.
method :: CardCode -> Name -> SkillIcon -> CardDef
method cardCode name icon =
  (encounterAsset_ cardCode name Set.AndThenThereWereNone)
    { cdCardTraits = singleton Method
    , cdSkills = [icon]
    }

anthonyMarston :: CardDef
anthonyMarston =
  victim ":and-then-there-were-none:011" ("Anthony Marston" <:> "The Playboy") [#agility, #wild]

edwardArmstrong :: CardDef
edwardArmstrong =
  victim ":and-then-there-were-none:012" ("Edward Armstrong" <:> "The Doctor") [#intellect, #wild]

emilyBrent :: CardDef
emilyBrent =
  victim ":and-then-there-were-none:013" ("Emily Brent" <:> "The Spinster") [#willpower, #intellect]

ethelRogers :: CardDef
ethelRogers =
  victim ":and-then-there-were-none:014" ("Ethel Rogers" <:> "The Housekeeper") [#willpower, #agility]

johnMacArthur :: CardDef
johnMacArthur =
  victim ":and-then-there-were-none:015" ("John MacArthur" <:> "The General") [#willpower, #combat]

lawrenceWargrave :: CardDef
lawrenceWargrave =
  victim ":and-then-there-were-none:016" ("Lawrence Wargrave" <:> "The Judge") [#willpower, #wild]

philipLombard :: CardDef
philipLombard =
  victim ":and-then-there-were-none:017" ("Philip Lombard" <:> "The Soldier") [#combat, #agility]

thomasRogers :: CardDef
thomasRogers =
  victim ":and-then-there-were-none:018" ("Thomas Rogers" <:> "The Butler") [#combat, #wild]

veraClaythorne :: CardDef
veraClaythorne =
  victim
    ":and-then-there-were-none:019"
    ("Vera Claythorne" <:> "The Governess")
    [#intellect, #agility]

williamBlore :: CardDef
williamBlore =
  victim ":and-then-there-were-none:020" ("William Blore" <:> "The Inspector") [#intellect, #combat]

blowpipe :: CardDef
blowpipe = method ":and-then-there-were-none:021" "Blowpipe" #agility

chloralHydrateOverdose :: CardDef
chloralHydrateOverdose =
  method ":and-then-there-were-none:022" "Chloral Hydrate Overdose" #willpower

crushedByDebris :: CardDef
crushedByDebris = method ":and-then-there-were-none:023" "Crushed by Debris" #combat

cyanideInjection :: CardDef
cyanideInjection = method ":and-then-there-were-none:024" "Cyanide Injection" #intellect

drownedInTheSea :: CardDef
drownedInTheSea = method ":and-then-there-were-none:025" "Drowned in the Sea" #agility

electrocution :: CardDef
electrocution = method ":and-then-there-were-none:026" "Electrocution" #intellect

fellFromAHeight :: CardDef
fellFromAHeight = method ":and-then-there-were-none:027" "Fell From a Height" #wild

hatchet :: CardDef
hatchet = method ":and-then-there-were-none:028" "Hatchet" #combat

improvisedItem :: CardDef
improvisedItem = method ":and-then-there-were-none:029" "Improvised Item" #wild

knife :: CardDef
knife = method ":and-then-there-were-none:030" "Knife" #wild

poisonedDrink :: CardDef
poisonedDrink = method ":and-then-there-were-none:031" "Poisoned Drink" #intellect

ropeNoose :: CardDef
ropeNoose = method ":and-then-there-were-none:032" "Rope Noose" #willpower

serviceRevolver :: CardDef
serviceRevolver = method ":and-then-there-were-none:033" "Service Revolver" #combat

smotheredInSleep :: CardDef
smotheredInSleep = method ":and-then-there-were-none:034" "Smothered in Sleep" #willpower

straightRazor :: CardDef
straightRazor = method ":and-then-there-were-none:035" "Straight Razor" #agility

-- | Four copies, set aside at setup; act 1b hangs them on the locations nearest the culprit.
antediluvianSigils :: CardDef
antediluvianSigils =
  (encounterAsset_ ":and-then-there-were-none:036" "Antediluvian Sigils" Set.AndThenThereWereNone)
    { cdCardTraits = setFromList [Ritual, Cursed]
    , cdEncounterSetQuantity = Just 4
    , cdUses = uses Offering 2
    }

-- | Resolution 2's memento. Set aside at setup, never shuffled in.
spectralConduit :: CardDef
spectralConduit =
  ( storyAsset
      ":and-then-there-were-none:037"
      ("Spectral Conduit" <:> "Whispers of the Lost")
      2
      Set.AndThenThereWereNone
  )
    { cdCardTraits = setFromList [Augury, Ritual]
    , cdSkills = [#willpower, #intellect, #wild]
    , cdSlots = [#arcane]
    , cdUses = uses Offering 4
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }
