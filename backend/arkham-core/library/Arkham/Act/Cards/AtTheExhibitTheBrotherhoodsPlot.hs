module Arkham.Act.Cards.AtTheExhibitTheBrotherhoodsPlot
  ( AtTheExhibitTheBrotherhoodsPlot(..)
  , atTheExhibitTheBrotherhoodsPlot
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Message

newtype AtTheExhibitTheBrotherhoodsPlot = AtTheExhibitTheBrotherhoodsPlot ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheExhibitTheBrotherhoodsPlot :: ActCard AtTheExhibitTheBrotherhoodsPlot
atTheExhibitTheBrotherhoodsPlot = act
  (2, A)
  AtTheExhibitTheBrotherhoodsPlot
  Cards.atTheExhibitTheBrotherhoodsPlot
  Nothing

instance RunMessage AtTheExhibitTheBrotherhoodsPlot where
  runMessage msg a@(AtTheExhibitTheBrotherhoodsPlot attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      pure a
    _ -> AtTheExhibitTheBrotherhoodsPlot <$> runMessage msg attrs
