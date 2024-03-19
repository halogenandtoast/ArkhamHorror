module Arkham.Treachery.Cards.MyriadForms
  ( myriadForms
  , MyriadForms(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MyriadForms = MyriadForms TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

myriadForms :: TreacheryCard MyriadForms
myriadForms = treachery MyriadForms Cards.myriadForms

instance RunMessage MyriadForms where
  runMessage msg t@(MyriadForms attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> MyriadForms <$> lift (runMessage msg attrs)
