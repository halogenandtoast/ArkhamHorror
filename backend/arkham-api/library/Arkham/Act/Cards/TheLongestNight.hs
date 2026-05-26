module Arkham.Act.Cards.TheLongestNight (theLongestNight) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheLongestNight = TheLongestNight ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theLongestNight :: ActCard TheLongestNight
theLongestNight = act (1, A) TheLongestNight Cards.theLongestNight Nothing

instance RunMessage TheLongestNight where
  runMessage msg a@(TheLongestNight attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheLongestNight <$> liftRunMessage msg attrs
