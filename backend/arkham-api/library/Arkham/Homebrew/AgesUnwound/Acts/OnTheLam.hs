module Arkham.Homebrew.AgesUnwound.Acts.OnTheLam (onTheLam) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card (genCard)
import Arkham.Deck qualified as Deck
import Arkham.ForMovement
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern ArkhamStreetsDeck)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
import Arkham.Movement qualified as Movement
import Arkham.Projection

newtype OnTheLam = OnTheLam ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: ActCard OnTheLam
onTheLam = act (1, A) OnTheLam Cards.onTheLam Nothing

{- | "a connecting location" -- and not a street that is itself being dealt.

A new Arkham Streets location is put into play facedown and is only revealed
when someone arrives, so an /unrevealed/ destination is already "a new Arkham
Streets location" and must not be offered this redirect: otherwise every card
that says "move to a new Arkham Streets location" would re-enter this window and
could keep dealing the deck down. Twisting Alleys reads its own condition the
same way.
-}
connectingDestination :: LocationMatcher
connectingDestination = ConnectedLocation ForMovement <> RevealedLocation

{- | Ability 2 is the parenthetical on ability 1, not an extra ability: "(You may
initiate a move even if there are no connecting locations in play.)"

The move action is published by each /accessible destination/, so with no
connection in play (the scenario's opening position: only Rivertown, and every
card it connects to is still in the Arkham Streets deck) there is no move action
to take. Ability 2 is that permission, and only that: it is restricted to the
case where no accessible location exists, so it never doubles up with a normal
move action, and to a non-empty Arkham Streets deck, so it is never offered when
the redirect would have nowhere to send you.

What it initiates is a move to the location you are already at, which ability 1
then retargets -- the move really does go through the reaction, rather than
ability 2 doing the move itself. A self-move is not legal, so the would-move
window is unskippable until the redirect has replaced it; see the 'Window.WouldMove'
case in "Arkham.Investigator.Runner". Ability 1's second window entry is scoped
to ability 2's own source: widening it to every destination instead would let
the redirect re-enter on the facedown street it just dealt.

Ability 2 is proxied onto 'YouSource' so that its button is drawn on the acting
investigator rather than on the act card, where a move action nobody is looking
for would be easy to miss. The act stays the ability's origin -- hence
'isProxySource' below, and the UI still highlights the act as what granted it.
-}
instance HasAbilities OnTheLam where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ triggered
          ( OrWindowMatcher
              [ WouldMove #when You #any Anywhere connectingDestination
              , WouldMove #when You (SourceIs $ a.ability 2) Anywhere YourLocation
              ]
          )
          Free
    , campaignI18n
        $ withI18nTooltip "onTheLam.move"
        $ restricted
          (proxied YouSource a)
          2
          (notExists AccessibleLocation <> ScenarioDeckWithCard ArkhamStreetsDeck)
        $ ActionAbility #move Nothing (ActionCost 1)
    , restricted a 3 (exists EmptyLocation) $ forced $ RoundEnds #when
    ]

instance RunMessage OnTheLam where
  runMessage msg a@(OnTheLam attrs) = runQueueT $ case msg of
    -- "[reaction] When an effect (including a move action) would allow you to
    -- move to a connecting location: Instead, move to a new Arkham Streets
    -- location."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      redirectToNewArkhamStreetsLocation (attrs.ability 1) iid
      pure a
    -- "(You may initiate a move even if there are no connecting locations in
    -- play.)" The move is seeded on your own location so that ability 1 has a
    -- movement to retarget; it is ability 1, not this one, that deals the street.
    --
    -- Built and pushed directly rather than with 'moveTo', which drops a move
    -- whose destination is where you already stand ('moveToEdit'). That guard is
    -- right for every other caller and is exactly what this ability needs to get
    -- around: the whole point of the self-move is to open a would-move window
    -- for the redirect to replace.
    UseThisAbility iid (isProxySource attrs -> True) 2 -> do
      lid <- getJustLocation iid
      push . Move =<< Movement.move (attrs.ability 2) iid lid
      pure a
    -- "Forced - At the end of the round: Shuffle each empty location into the
    -- Arkham Streets deck." Empty is the rules' sense: no investigators and no
    -- enemies at it.
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      selectEach EmptyLocation \lid -> do
        card <- field Field.LocationCard lid
        -- Not `removeLocation`: that diverts a clueless Victory X location into
        -- the victory display instead of removing it, and French Hill is
        -- Victory 1. Shuffling a location back is not overcoming it.
        removeLocationWithoutVictory lid
        shuffleCardsIntoDeck (Deck.ScenarioDeckByKey ArkhamStreetsDeck) [card]
      pure a
    -- "Lost in Darkness. Flip back to act 1a." Nothing in the scenario advances
    -- this act -- agenda 1b sends the table to act 2a -- so this is the
    -- no-op loop guard the card prints for anything that does.
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push . ReplaceAct attrs.id =<< genCard Cards.onTheLam
      pure a
    _ -> OnTheLam <$> liftRunMessage msg attrs
