module Arkham.Homebrew.DarkMatter.Agendas.NostalgiaAgendas (
  theNostalgiaII,
  theThingFromEarth,
  screamOfTheDead,
  itsWeirdAndPissedOff,
) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card.CardCode (toCardCode)
import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window (assetLeavingPlay)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Helpers (
  getScanningDeck,
  scanAction,
  scanIcons,
  scanTopOfScanningDeck,
 )
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Crew))

{- | All four "In the Shadow of Earth" agendas share their front text:

"[action] Scan. If the top card of the scanning deck has an icon matching your
current location, spend 1[per_investigator] clues, as a group: Draw the top card
of the scanning deck.
Forced - When a [[Crew]] story asset is defeated or discarded: Remove it from the
game."

Agenda 4 replaces the Forced clause: the crew asset is attached face down to The
Entity instead of being removed.

The icon clause is a /restriction on using the ability/, not a mid-effect
condition, and the clue spend is an ordinary declared cost. Campaign guide,
"Motion Scanning" (scenario IIIb, docs/homebrew/data/dm-guide-pp11-20.md):

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
newtype NostalgiaAgenda = NostalgiaAgenda AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkNostalgia :: Int -> CardDef -> GameValue -> AgendaCard NostalgiaAgenda
mkNostalgia n def = agenda (n, A) NostalgiaAgenda def

theNostalgiaII :: AgendaCard NostalgiaAgenda
theNostalgiaII = mkNostalgia 1 Cards.theNostalgiaII (Static 3)

theThingFromEarth :: AgendaCard NostalgiaAgenda
theThingFromEarth = mkNostalgia 2 Cards.theThingFromEarth (Static 4)

screamOfTheDead :: AgendaCard NostalgiaAgenda
screamOfTheDead = mkNostalgia 3 Cards.screamOfTheDead (Static 3)

itsWeirdAndPissedOff :: AgendaCard NostalgiaAgenda
itsWeirdAndPissedOff = mkNostalgia 4 Cards.itsWeirdAndPissedOff (Static 2)

-- | Only the final agenda attaches defeated crew to The Entity.
isFinalAgenda :: AgendaAttrs -> Bool
isFinalAgenda a = toCardCode (toCardDef a) == toCardCode Cards.itsWeirdAndPissedOff

{- | Marks the locations you are currently allowed to motion scan from, i.e.
those whose icon appears on the back of the top card of the scanning deck.
Ability criteria are pure, so the dynamic "matching the top card" half of the
restriction is computed here and read back as a location modifier.
-}
motionScannable :: ModifierType
motionScannable = ScenarioModifier "motionScannable"

instance HasModifiersFor NostalgiaAgenda where
  getModifiersFor (NostalgiaAgenda a) =
    getScanningDeck >>= \case
      [] -> pure ()
      top : _ -> modifySelect a (mapOneOf LocationWithSymbol (scanIcons top)) [motionScannable]

instance HasAbilities NostalgiaAgenda where
  getAbilities (NostalgiaAgenda a) =
    [ restricted a 1 (exists $ YourLocation <> LocationWithModifier motionScannable)
        $ scanAction (GroupClueCost (PerPlayer 1) Anywhere)
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (AssetWithTrait Crew)
    ]

instance RunMessage NostalgiaAgenda where
  runMessage msg a@(NostalgiaAgenda attrs) = runQueueT $ case msg of
    -- The clue cost has already been paid as a group by the time we get here,
    -- and the restriction guarantees the top card matches, so the scan just
    -- draws it. 'scanTopOfScanningDeck' also removes the card from the scanning
    -- deck and fires the scan window that the act objective ("when you draw a
    -- story asset from the scanning deck, advance") and the Evidence cards
    -- listen for.
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scanTopOfScanningDeck iid (attrs.ability 1)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (assetLeavingPlay -> aid) _ -> do
      if isFinalAgenda attrs
        then
          selectOne (enemyIs Enemies.theEntity)
            >>= traverse_ (push . PlaceAsset aid . AttachedToEnemy)
        else push $ RemoveFromGame (AssetTarget aid)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> NostalgiaAgenda <$> liftRunMessage msg attrs
