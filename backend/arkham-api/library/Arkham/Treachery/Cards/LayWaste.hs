module Arkham.Treachery.Cards.LayWaste (layWaste) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LayWaste = LayWaste TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

layWaste :: TreacheryCard LayWaste
layWaste = treachery LayWaste Cards.layWaste

-- TODO: abilities
instance RunMessage LayWaste where
  runMessage msg (LayWaste attrs) = runQueueT $ LayWaste <$> liftRunMessage msg attrs
