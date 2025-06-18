module Arkham.Scenarios.TheMidwinterGala.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Message (Message (ScenarioSpecific))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenario.Deck

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ scope "theMidwinterGala" a

becomeSpellbound :: (ReverseQueue m, AsId a, IdOf a ~ AssetId) => a -> m ()
becomeSpellbound a = forTarget (asId a) (ScenarioSpecific "spellbound" Null)

-- spellbound :: ReverseQueue m => AssetId -> m ()
-- spellbound a = gameModifier ScenarioSource a (ScenarioModifier "spellbound")

getGuestDeck :: HasGame m => m [Card]
getGuestDeck = getScenarioDeck GuestDeck

shuffleGuestDeck :: ReverseQueue m => m ()
shuffleGuestDeck = shuffleDeck GuestDeck

shuffleIntoGuestDeck
  :: (IsCard (Element cards), ReverseQueue m, MonoFoldable cards)
  => cards
  -> m ()
shuffleIntoGuestDeck cs = shuffleCardsIntoDeck GuestDeck $ map toCard $ toList cs
