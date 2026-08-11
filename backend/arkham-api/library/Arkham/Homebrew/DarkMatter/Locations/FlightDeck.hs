module Arkham.Homebrew.DarkMatter.Locations.FlightDeck (flightDeck) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FlightDeck = FlightDeck LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flightDeck :: LocationCard FlightDeck
flightDeck = location FlightDeck Cards.flightDeck 3 (PerPlayer 1)

{- | "[action] Deal 2 horror to each investigator at this location: Gain 2 clues
from the token bank. (Limit once per round.)"
-}
instance HasAbilities FlightDeck where
  getAbilities (FlightDeck a) =
    extendRevealed1 a $ playerLimit PerRound $ restricted a 1 Here actionAbility

instance RunMessage FlightDeck where
  runMessage msg l@(FlightDeck attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      here <- select $ investigatorAt attrs.id
      for_ here \iid' -> assignHorror iid' (attrs.ability 1) 2
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> FlightDeck <$> liftRunMessage msg attrs
