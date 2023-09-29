module Arkham.Event.Cards.BurningTheMidnightOil (
  burningTheMidnightOil,
  BurningTheMidnightOil (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigate

newtype BurningTheMidnightOil = BurningTheMidnightOil EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burningTheMidnightOil :: EventCard BurningTheMidnightOil
burningTheMidnightOil = event BurningTheMidnightOil Cards.burningTheMidnightOil

instance RunMessage BurningTheMidnightOil where
  runMessage msg e@(BurningTheMidnightOil attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigation <- mkInvestigate iid attrs
      pushAll
        [ TakeResources iid 2 (toSource attrs) False
        , toMessage investigation
        ]
      pure e
    _ -> BurningTheMidnightOil <$> runMessage msg attrs
