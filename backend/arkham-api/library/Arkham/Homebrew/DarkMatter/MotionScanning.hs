module Arkham.Homebrew.DarkMatter.MotionScanning (
  motionScannable,
  motionScanModifiers,
  motionScanAbilities,
) where

import Arkham.Ability
import Arkham.Card.CardCode (HasCardCode)
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (ScenarioModifier), modifySelect)
import Arkham.Homebrew.DarkMatter.Helpers (getScanningDeck, scanAction, scanIcons)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Source
import Arkham.Trait (Trait (Crew))

{- | The front side shared by all four "In the Shadow of Earth" agendas:

"[action] Scan. If the top card of the scanning deck has an icon matching your
current location, spend 1[per_investigator] clues, as a group: Draw the top card
of the scanning deck.
Forced - When a [[Crew]] story asset is defeated or discarded: Remove it from the
game."

Agenda 4 replaces the Forced clause: the crew asset is attached face down to The
Entity instead of being removed. Only the /handler/ differs, so all four agendas
advertise the same two abilities.

The icon clause is a /restriction on using the ability/, not a mid-effect
condition, and the clue spend is an ordinary declared cost. Campaign guide,
"Motion Scanning" (scenario IIIb, docs\/homebrew\/data\/dm-guide-pp11-20.md):

  "You may only scan when you are at a location with an icon matching the top
  card of the scanning deck. When you scan, instead of searching for the topmost
  card of the scanning deck matching the icon you are scanning for, simply draw
  the top card of the scanning deck."

The icons are printed on the /back/ of scanning cards ("Some encounter cards in
this campaign will have icons indicated on the bottom of the card when they are
face down", guide p2), so the top card's icons are public information even
though the card's identity is not — the whole point of the scenario is that you
"follow the icons depicted on the top card of the scanning deck". So the check
happens before payment, exactly like any other ability restriction, and the cost
can be declared on the ability the same way Sophie declares it for her own Scan
("[action] Spend 1[per_investigator] clues: Scan ... as if you were at any
location" — Sophie is the same cost with the location restriction lifted).
-}

{- | Marks the locations you are currently allowed to motion scan from, i.e.
those whose icon appears on the back of the top card of the scanning deck.
Ability criteria are pure, so the dynamic "matching the top card" half of the
restriction is computed here and read back as a location modifier.
-}
motionScannable :: ModifierType
motionScannable = ScenarioModifier "motionScannable"

motionScanModifiers :: (HasModifiersM m, Sourceable source) => source -> m ()
motionScanModifiers source =
  getScanningDeck >>= \case
    [] -> pure ()
    top : _ -> modifySelect source (mapOneOf LocationWithSymbol (scanIcons top)) [motionScannable]

motionScanAbilities :: (HasCardCode a, Sourceable a) => a -> [Ability]
motionScanAbilities a =
  [ restricted a 1 (exists $ YourLocation <> LocationWithModifier motionScannable)
      $ scanAction (GroupClueCost (PerPlayer 1) Anywhere)
  , mkAbility a 2 $ forced $ AssetLeavesPlay #when (AssetWithTrait Crew)
  ]
