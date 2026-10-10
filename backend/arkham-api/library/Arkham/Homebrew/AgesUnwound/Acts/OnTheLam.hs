module Arkham.Homebrew.AgesUnwound.Acts.OnTheLam (onTheLam) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card (genCard)
import Arkham.Deck qualified as Deck
import Arkham.ForMovement
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern ArkhamStreetsDeck)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
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

There is nothing for the engine to hang that on -- the move action is published
by each /accessible destination/, so with no connection in play (which is the
scenario's opening position: only Rivertown, and every card it connects to is
still in the Arkham Streets deck) no move action exists for ability 1 to
redirect. Ability 2 is that permission, and only that: it is restricted to the
case where no accessible location exists, so it never doubles up with a normal
move action, and to a non-empty Arkham Streets deck, so it is never offered for
a move that would immediately be cancelled.
-}
instance HasAbilities OnTheLam where
  getAbilities (OnTheLam a) =
    [ mkAbility a 1
        $ triggered (WouldMove #when You #any Anywhere connectingDestination) Free
    , restricted a 2 (notExists AccessibleLocation <> ScenarioDeckWithCard ArkhamStreetsDeck)
        $ ActionAbility #move Nothing (ActionCost 1)
    , mkAbility a 3 $ forced $ RoundEnds #when
    ]

instance RunMessage OnTheLam where
  runMessage msg a@(OnTheLam attrs) = runQueueT $ case msg of
    -- "[reaction] When an effect (including a move action) would allow you to
    -- move to a connecting location: Instead, move to a new Arkham Streets
    -- location."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      redirectToNewArkhamStreetsLocation (attrs.ability 1) iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      moveToNewArkhamStreetsLocation (attrs.ability 2) iid
      pure a
    -- "Forced - At the end of the round: Shuffle each empty location into the
    -- Arkham Streets deck." Empty is the rules' sense: no investigators and no
    -- enemies at it.
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      selectEach EmptyLocation \lid -> do
        card <- field Field.LocationCard lid
        removeLocation lid
        shuffleCardsIntoDeck (Deck.ScenarioDeckByKey ArkhamStreetsDeck) [card]
      pure a
    -- "Lost in Darkness. Flip back to act 1a." Nothing in the scenario advances
    -- this act -- agenda 1b sends the table to act 2a -- so this is the
    -- no-op loop guard the card prints for anything that does.
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push . ReplaceAct attrs.id =<< genCard Cards.onTheLam
      pure a
    _ -> OnTheLam <$> liftRunMessage msg attrs
