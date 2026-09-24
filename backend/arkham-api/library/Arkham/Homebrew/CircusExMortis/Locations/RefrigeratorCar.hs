module Arkham.Homebrew.CircusExMortis.Locations.RefrigeratorCar (refrigeratorCar) where

import Arkham.Card (cardMatch, card_)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe)
import Arkham.History
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RefrigeratorCar = RefrigeratorCar LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

refrigeratorCar :: LocationCard RefrigeratorCar
refrigeratorCar = symbolLabel $ location RefrigeratorCar Cards.refrigeratorCar 4 (Static 2)

instance HasModifiersFor RefrigeratorCar where
  getModifiersFor (RefrigeratorCar a) = do
    noClues <- a.id `matches` LocationWithoutClues
    when noClues $ modifySelectMaybe a (investigatorAt a) \iid -> do
      cards <- getHistoryField #round iid HistoryPlayedCards
      guard $ noneBy [(== Just a.id) . playedCardLocation, (`cardMatch` card_ #asset)] cards
      pure [ReduceCostOf #asset 1]
