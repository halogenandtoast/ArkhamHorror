module Arkham.Treachery.Cards.FigureInTheShadows (figureInTheShadows) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FigureInTheShadows = FigureInTheShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

figureInTheShadows :: TreacheryCard FigureInTheShadows
figureInTheShadows = treachery FigureInTheShadows Cards.figureInTheShadows

instance RunMessage FigureInTheShadows where
  runMessage msg t@(FigureInTheShadows attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> FigureInTheShadows <$> liftRunMessage msg attrs
