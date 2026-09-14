module Arkham.Homebrew.CircusExMortis.Locations.RefrigeratorCar (refrigeratorCar) where

import Arkham.Card (cardMatch)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe)
import Arkham.History
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RefrigeratorCar = RefrigeratorCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

refrigeratorCar :: LocationCard RefrigeratorCar
refrigeratorCar = location RefrigeratorCar Cards.refrigeratorCar 4 (Static 2)

instance HasModifiersFor RefrigeratorCar where
  getModifiersFor (RefrigeratorCar a) = do
    noClues <- a.id <=~> LocationWithoutClues
    when noClues $ modifySelectMaybe a (investigatorAt a) \iid -> do
      playedCards <- lift $ historyPlayedCards <$> getHistory RoundHistory iid
      let assetMatcher = #asset :: CardMatcher
      guard $ none (`cardMatch` assetMatcher) playedCards
      pure [ReduceCostOf assetMatcher 1]

instance HasAbilities RefrigeratorCar where
  getAbilities (RefrigeratorCar a) = extendRevealed a []

instance RunMessage RefrigeratorCar where
  runMessage msg (RefrigeratorCar attrs) = runQueueT $ RefrigeratorCar <$> liftRunMessage msg attrs
