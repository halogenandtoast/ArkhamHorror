module Arkham.Treachery.Cards.VisionsOfFuturesPast (visionsOfFuturesPast) where

import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VisionsOfFuturesPast = VisionsOfFuturesPast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

visionsOfFuturesPast :: TreacheryCard VisionsOfFuturesPast
visionsOfFuturesPast = treachery VisionsOfFuturesPast Cards.visionsOfFuturesPast

instance RunMessage VisionsOfFuturesPast where
  runMessage msg t@(VisionsOfFuturesPast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      discardTopOfDeck iid attrs n
      pure t
    _ -> VisionsOfFuturesPast <$> liftRunMessage msg attrs
