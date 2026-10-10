{- | Scenario I's signature mechanic, in one place.

> __Arkham Streets.__ Several scenario effects will ask you to move to a new
> Arkham Streets location. To do this, put the top card of the Arkham Streets
> deck into play facedown, then move to it. If there are no locations remaining
> in the Arkham Streets deck, cancel the effect of the move.

Eight cards print some form of that instruction, so none of them re-derives it:
they call 'moveToNewArkhamStreetsLocation' (a move the card creates) or
'redirectToNewArkhamStreetsLocation' (a move already in flight that this card
replaces). The empty-deck branch differs between the two, which is the whole
reason there are two of them -- a move that was never created has nothing to
cancel, while an in-flight one has to be cancelled explicitly.

Scenario-local on purpose: nothing outside Night of Fire has an Arkham Streets
deck, so this does not belong in the campaign's shared @Helpers.hs@.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, push)
import Arkham.Helpers.Movement (replaceMovement)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Homebrew.AgesUnwound.Helpers (scenarioI18n)
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern ArkhamStreetsDeck)
import Arkham.I18n
import Arkham.Id
import Arkham.Message (Message (RemoveCardFromScenarioDeck))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Movement (Destination (ToLocation), Movement (moveDestination))
import Arkham.Prelude
import Arkham.Source
import Control.Monad.Trans.Class (MonadTrans)

-- | Every Night of Fire card and the scenario itself share this i18n scope.
nightOfFireI18n :: (HasI18n => a) -> a
nightOfFireI18n = scenarioI18n "nightOfFire"

-- | The Arkham Streets deck, top card first.
getArkhamStreetsDeck :: HasGame m => m [Card]
getArkhamStreetsDeck = getScenarioDeck ArkhamStreetsDeck

{- | "put the top card of the Arkham Streets deck into play facedown" --
'Nothing' when there are no locations remaining in it.

The card is already a real card in the scenario deck, so it is placed as-is and
struck from the deck; minting a new one would leave the original behind (see
@project_place_location_card_leaves_the_set_aside_copy@).
-}
putNewArkhamStreetsLocationIntoPlay :: ReverseQueue m => m (Maybe LocationId)
putNewArkhamStreetsLocationIntoPlay =
  getArkhamStreetsDeck >>= \case
    [] -> pure Nothing
    card : _ -> do
      push $ RemoveCardFromScenarioDeck ArkhamStreetsDeck card
      Just <$> placeLocation card

{- | "move to a new Arkham Streets location", for a card that creates the move
itself (Just Business, the [cultist] token, Rivertown's neighbours). With an
empty deck the move is cancelled by never happening.
-}
moveToNewArkhamStreetsLocation
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
moveToNewArkhamStreetsLocation source iid =
  putNewArkhamStreetsLocationIntoPlay >>= traverse_ (moveTo source iid)

{- | "Instead, move to a new Arkham Streets location" -- for a card that
replaces a move that is already in flight (On the Lam, Getting Your Bearings,
Twisting Alleys). Retargets the live movement rather than queueing a second
one, so the original destination's leave/enter windows are rewritten with it.

With an empty deck there *is* a move to cancel, and Twisting Alleys says so out
loud: "If no such locations exist, instead cancel the effects of the move."
-}
redirectToNewArkhamStreetsLocation
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m), Sourceable source)
  => source -> InvestigatorId -> t m ()
redirectToNewArkhamStreetsLocation source iid =
  putNewArkhamStreetsLocationIntoPlay >>= \case
    Nothing -> cancelMovement source iid
    Just lid -> replaceMovement iid \m -> m {moveDestination = ToLocation lid}
