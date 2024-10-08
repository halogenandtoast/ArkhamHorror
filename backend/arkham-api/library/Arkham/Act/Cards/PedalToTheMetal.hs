module Arkham.Act.Cards.PedalToTheMetal
  ( PedalToTheMetal(..)
  , pedalToTheMetal
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype PedalToTheMetal = PedalToTheMetal ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pedalToTheMetal :: ActCard PedalToTheMetal
pedalToTheMetal = act (1, A) PedalToTheMetal Cards.pedalToTheMetal Nothing

instance RunMessage PedalToTheMetal where
  runMessage msg a@(PedalToTheMetal attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> PedalToTheMetal <$> liftRunMessage msg attrs
