module Arkham.Treachery.Cards.BreakingPoint (breakingPoint) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.CardDefs.Core2026 qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BreakingPoint = BreakingPoint TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingPoint :: TreacheryCard BreakingPoint
breakingPoint = treachery BreakingPoint Cards.breakingPoint

instance RunMessage BreakingPoint where
  runMessage msg t@(BreakingPoint attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      batched \_ -> do
        directDamage iid attrs 1
        doStep 1 msg
      pure t
    DoStep 1 msg'@(Revelation iid (isSource attrs -> True)) -> do
      remainingSanity <- field InvestigatorRemainingSanity iid
      when (remainingSanity <= 6) do
        batched \_ -> do
          directDamage iid attrs 1
          doStep 2 msg'
      pure t
    DoStep 2 (Revelation iid (isSource attrs -> True)) -> do
      remainingSanity <- field InvestigatorRemainingSanity iid
      when (remainingSanity <= 3) $ directDamage iid attrs 1
      pure t
    _ -> BreakingPoint <$> liftRunMessage msg attrs
