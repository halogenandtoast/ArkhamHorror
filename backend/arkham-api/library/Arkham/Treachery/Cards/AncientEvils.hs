module Arkham.Treachery.Cards.AncientEvils (ancientEvils) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientEvils = AncientEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientEvils :: TreacheryCard AncientEvils
ancientEvils = treachery AncientEvils Cards.ancientEvils

instance RunMessage AncientEvils where
  runMessage msg t@(AncientEvils attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> AncientEvils <$> liftRunMessage msg attrs
