module Arkham.Scenarios.TheMidwinterGala.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Message.Lifted
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Source

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ scope "theMidwinterGala" a

spellbound :: ReverseQueue m => AssetId -> m ()
spellbound a = gameModifier ScenarioSource a (ScenarioModifier "spellbound")

getGuestDeck :: HasGame m => m [Card]
getGuestDeck = getScenarioDeck GuestDeck

shuffleGuestDeck :: ReverseQueue m => m ()
shuffleGuestDeck = shuffleDeck GuestDeck

shuffleIntoGuestDeck
  :: (IsCard (Element cards), ReverseQueue m, MonoFoldable cards)
  => cards
  -> m ()
shuffleIntoGuestDeck cs = shuffleCardsIntoDeck GuestDeck $ map toCard $ toList cs
