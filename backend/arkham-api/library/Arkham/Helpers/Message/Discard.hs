module Arkham.Helpers.Message.Discard where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Discard
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window
import Control.Monad.Trans.Class

discardFromHand
  :: Sourceable source
  => InvestigatorId
  -> source
  -> DiscardStrategy
  -> Int
  -> HandDiscard Message
discardFromHand iid (toSource -> source) strategy amount =
  HandDiscard
    { discardStrategy = strategy
    , discardFilter = AnyCard
    , discardSource = source
    , discardAmount = amount
    , discardThen = Nothing
    , discardTarget = Nothing
    , discardInvestigator = iid
    , discardDestination = ToDiscardPile
    }

chooseAndDiscardCard
  :: Sourceable source => InvestigatorId -> source -> HandDiscard Message
chooseAndDiscardCard iid source = discardFromHand iid source DiscardChoose 1

discardCard
  :: (Sourceable source, IsCard card)
  => InvestigatorId
  -> source
  -> card
  -> HandDiscard Message
discardCard iid source (toCardId -> cardId) =
  (chooseAndDiscardCard iid source) {discardFilter = CardWithId cardId}

randomDiscard
  :: Sourceable source => InvestigatorId -> source -> HandDiscard Message
randomDiscard iid source = randomDiscardMatching iid source AnyCard

randomDiscardN
  :: Sourceable source => InvestigatorId -> source -> Int -> HandDiscard Message
randomDiscardN iid source n = (randomDiscard iid source) {discardAmount = n}

randomDiscardMatching
  :: Sourceable source
  => InvestigatorId
  -> source
  -> CardMatcher
  -> HandDiscard Message
randomDiscardMatching iid source matcher =
  (discardFromHand iid source DiscardRandom 1) {discardFilter = matcher}

discardAll
  :: Sourceable source
  => InvestigatorId
  -> source
  -> CardMatcher
  -> HandDiscard Message
discardAll iid (toSource -> source) matcher =
  HandDiscard
    { discardStrategy = DiscardAll
    , discardFilter = matcher
    , discardSource = source
    , discardAmount = 0
    , discardThen = Nothing
    , discardTarget = Nothing
    , discardInvestigator = iid
    , discardDestination = ToDiscardPile
    }

-- | Walk a list of windows and, for each `WouldDiscardFromHand` window
-- targeting `iid`, rewrite the queued `Do (DiscardFromHand ...)` via `f`.
-- Use this to react to a discard the game is about to do — for example,
-- redirect it to the top of the deck, change its amount, or otherwise
-- rewrite the discard before it resolves.
updateWouldDiscardFromHand
  :: (MonadTrans t, HasQueue Message m)
  => InvestigatorId
  -> [Window]
  -> (HandDiscard Message -> HandDiscard Message)
  -> t m ()
updateWouldDiscardFromHand iid ws f = lift $ for_ ws \w -> case windowType w of
  Window.WouldDiscardFromHand iid' source | iid == iid' ->
    replaceMessageMatching
      \case
        Do (DiscardFromHand handDiscard) ->
          handDiscard.investigator == iid && handDiscard.source == source
        _ -> False
      \case
        Do (DiscardFromHand handDiscard) -> [Do (DiscardFromHand (f handDiscard))]
        _ -> []
  _ -> pure ()
