module Arkham.Location.Cards.WindsorPalaceHotel (windsorPalaceHotel) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.DogsOfWar.Helpers

newtype WindsorPalaceHotel = WindsorPalaceHotel LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

windsorPalaceHotel :: LocationCard WindsorPalaceHotel
windsorPalaceHotel = symbolLabel $ location WindsorPalaceHotel Cards.windsorPalaceHotel 2 (PerPlayer 1)

instance HasModifiersFor WindsorPalaceHotel where
  getModifiersFor (WindsorPalaceHotel a) = do
    hasKeyLocus <- matches a.id locationWithKeyLocus
    modifySelect
      a
      (investigatorAt a)
      [ if hasKeyLocus
          then IncreaseCostOf (oneOf [#ally, #item]) 1
          else ReduceCostOf (oneOf [#ally, #item]) 1
      ]
