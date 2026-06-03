module Arkham.Location.Cards.Milkhouse (milkhouse) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.TheLongestNight.Helpers
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (getBatchId, windowType)
import Arkham.Window qualified as Window

newtype Milkhouse = Milkhouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

milkhouse :: LocationCard Milkhouse
milkhouse = symbolLabel $ locationWith Milkhouse Cards.milkhouse 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Milkhouse where
  getAbilities (Milkhouse a) =
    extendRevealed
      a
      [ mkAbility a 1
          $ freeReaction
          $ oneOf
            [ ScenarioEvent #when Nothing ("wouldPlaceDecoy:" <> tshow a.id)
            , ScenarioEvent #when Nothing ("wouldPlaceTrap:" <> tshow a.id)
            ]
      , groupLimit PerRound
          $ restricted a 2 Here actionAbility
      ]

instance RunMessage Milkhouse where
  runMessage msg l@(Milkhouse attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 ws _ -> do
      let (source', cannotHave, tType) = getWouldPlaceDetails ws
      lead <- getLead
      destinations <- select $ connectedTo (be attrs) <> LocationWithoutModifier cannotHave
      when (notNull destinations) do
        push $ CancelBatch $ getBatchId ws
        chooseTargetM lead destinations \toLid ->
          placeTokens source' toLid tType 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      fireCards <- getSetAsideCardsMatching (cardIs Treacheries.fire)
      case fireCards of
        (x : rest) -> do
          drawCard iid x
          case rest of
            (y : _) -> shuffleCardsIntoDeck Deck.EncounterDeck [y]
            [] -> pure ()
        [] -> pure ()
      pure l
    _ -> Milkhouse <$> liftRunMessage msg attrs

getWouldPlaceDetails :: HasCallStack => [Window.Window] -> (Source, ModifierType, Token)
getWouldPlaceDetails = go
 where
  go [] = error "invalid window"
  go ((windowType -> Window.ScenarioEvent key _ val) : _)
    | "wouldPlaceTrap:" `isPrefixOf` key =
        let (s, _lid :: LocationId) = toResult val in (s, CannotHaveTraps, Damage)
    | "wouldPlaceDecoy:" `isPrefixOf` key =
        let (s, _lid :: LocationId) = toResult val in (s, CannotHaveDecoys, Horror)
  go (_ : rest) = go rest
