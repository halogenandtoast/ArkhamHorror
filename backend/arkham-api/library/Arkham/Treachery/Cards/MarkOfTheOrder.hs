module Arkham.Treachery.Cards.MarkOfTheOrder (markOfTheOrder) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MarkOfTheOrder = MarkOfTheOrder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markOfTheOrder :: TreacheryCard MarkOfTheOrder
markOfTheOrder = treachery MarkOfTheOrder Cards.markOfTheOrder

instance RunMessage MarkOfTheOrder where
  runMessage msg t@(MarkOfTheOrder attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectEach (InvestigatorWithTokenKey #elderthing) \i -> loseResources i attrs 3
      selectEach (InvestigatorWithTokenKey #tablet) (`randomDiscard` attrs)
      selectEach (InvestigatorWithTokenKey #cultist) \i -> assignHorror i attrs 1
      selectEach (InvestigatorWithTokenKey #skull) \i -> assignDamage i attrs 1
      pure t
    _ -> MarkOfTheOrder <$> liftRunMessage msg attrs
