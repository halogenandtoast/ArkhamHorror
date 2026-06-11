module Arkham.Treachery.Cards.FeastOfLocusts (feastOfLocusts) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FeastOfLocusts = FeastOfLocusts TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feastOfLocusts :: TreacheryCard FeastOfLocusts
feastOfLocusts = treachery FeastOfLocusts Cards.feastOfLocusts

instance RunMessage FeastOfLocusts where
  runMessage msg t@(FeastOfLocusts attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      swarmCards <- selectCount IsSwarm
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed $ 2 + swarmCards)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs $ if n >= 4 then 3 else 2
      pure t
    _ -> FeastOfLocusts <$> liftRunMessage msg attrs
