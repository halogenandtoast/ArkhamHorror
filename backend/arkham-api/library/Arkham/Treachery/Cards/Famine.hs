module Arkham.Treachery.Cards.Famine (famine) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Famine = Famine TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

famine :: TreacheryCard Famine
famine = treachery Famine Cards.famine

instance HasModifiersFor Famine where
  getModifiersFor (Famine a) =
    modifySelect a Anyone [IncreaseCostOf (basic AnyCard) 1]

instance HasAbilities Famine where
  getAbilities (Famine a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage Famine where
  runMessage msg t@(Famine attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Famine <$> liftRunMessage msg attrs
