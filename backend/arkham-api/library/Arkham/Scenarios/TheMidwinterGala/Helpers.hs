module Arkham.Scenarios.TheMidwinterGala.Helpers where

import Arkham.Prelude
import Arkham.I18n
import Arkham.Message.Lifted
import Arkham.Id
import Arkham.Scenario.Deck
import Arkham.Helpers.Scenario

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ scope "theMidwinterGala" a

spellbound :: ReverseQueue m => AssetId -> m ()
spellbound = push . BecomeSpellbound

getGuestDeck :: HasGame m => m [Card]
getGuestDeck = getScenarioDeck GuestDeck

shuffleGuestDeck :: ReverseQueue m => m ()
shuffleGuestDeck = shuffleDeck GuestDeck

shuffleIntoGuestDeck
  :: (IsCard (Element cards), ReverseQueue m, MonoFoldable cards)
  => cards
  -> m ()
shuffleIntoGuestDeck cs = shuffleCardsIntoDeck GuestDeck $ map toCard $ toList cs
