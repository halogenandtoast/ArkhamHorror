module Arkham.Scenarios.TheMidwinterGala.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Modifiers (modifySelfWhen)
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher.Asset
import Arkham.Message (Message (ScenarioSpecific))
import Arkham.Message.Lifted
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Source
import Arkham.Target
import Control.Monad.Writer.Class (MonadWriter)
import Data.Map.Monoidal.Strict (MonoidalMap)
import GHC.Records

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "theMidwinterGala" a

becomeSpellbound :: (ReverseQueue m, AsId a, IdOf a ~ AssetId) => a -> m ()
becomeSpellbound a = forTarget (asId a) (ScenarioSpecific "spellbound" Null)

{- | SpellboundAsset
Matches an asset that has the "spellbound" modifier, ignoring its
visibility since those are normally hidden.
-}
pattern SpellboundAsset :: AssetMatcher -> AssetMatcher
pattern SpellboundAsset inner =
  IgnoreVisibility (AssetMatches [AssetWithModifier (ScenarioModifier "spellbound"), inner])

getGuestDeck :: HasGame m => m [Card]
getGuestDeck = getScenarioDeck GuestDeck

shuffleGuestDeck :: ReverseQueue m => m ()
shuffleGuestDeck = shuffleDeck GuestDeck

shuffleIntoGuestDeck
  :: (IsCard (Element cards), ReverseQueue m, MonoFoldable cards)
  => cards
  -> m ()
shuffleIntoGuestDeck cs = shuffleCardsIntoDeck GuestDeck $ map toCard $ toList cs

handleSpellbound
  :: ( HasGame m
     , Targetable a
     , Sourceable a
     , HasField "meta" a Value
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => a -> m ()
handleSpellbound a = do
  let spellbound = toResultDefault False a.meta
  modifySelfWhen a spellbound [ScenarioModifier "spellbound"]
