module Arkham.Location.Cards.Milkhouse (milkhouse) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype Milkhouse = Milkhouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

milkhouse :: LocationCard Milkhouse
milkhouse = symbolLabel $ locationWith Milkhouse Cards.milkhouse 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Milkhouse where
  getAbilities (Milkhouse a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here actionAbility

instance RunMessage Milkhouse where
  runMessage msg l@(Milkhouse attrs) = runQueueT $ case msg of
    PlaceTokens source target tType n
      | isTarget attrs target && tType `elem` [Horror, Damage] -> do
          lead <- getLead
          connected <- select $ connectedTo (be attrs)
          if null connected
            then Milkhouse <$> liftRunMessage msg attrs
            else do
              chooseTargetM lead connected \lid ->
                push $ PlaceTokens source (toTarget lid) tType n
              pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
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
