module Arkham.Treachery.Cards.ParadoxEffect (paradoxEffect) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ParadoxEffect = ParadoxEffect TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradoxEffect :: TreacheryCard ParadoxEffect
paradoxEffect = treachery ParadoxEffect Cards.paradoxEffect

instance RunMessage ParadoxEffect where
  runMessage msg t@(ParadoxEffect attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs n
      pure t
    _ -> ParadoxEffect <$> liftRunMessage msg attrs
