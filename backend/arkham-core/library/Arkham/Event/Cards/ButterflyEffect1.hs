module Arkham.Event.Cards.ButterflyEffect1 (butterflyEffect1, ButterflyEffect1 (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Prelude

newtype ButterflyEffect1 = ButterflyEffect1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

butterflyEffect1 :: EventCard ButterflyEffect1
butterflyEffect1 = event ButterflyEffect1 Cards.butterflyEffect1

instance RunMessage ButterflyEffect1 where
  runMessage msg e@(ButterflyEffect1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      _ <- getSkillTestInvestigator
      _ <- getCommittableCards iid

      pure e
    _ -> ButterflyEffect1 <$> runMessage msg attrs
