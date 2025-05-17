module Arkham.Treachery.Cards.OozeAndFilth (oozeAndFilth) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OozeAndFilth = OozeAndFilth TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oozeAndFilth :: TreacheryCard OozeAndFilth
oozeAndFilth = treachery OozeAndFilth Cards.oozeAndFilth

instance HasModifiersFor OozeAndFilth where
  getModifiersFor (OozeAndFilth a) = modifySelect a Anywhere [ShroudModifier 1]

instance HasAbilities OozeAndFilth where
  getAbilities (OozeAndFilth a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage OozeAndFilth where
  runMessage msg t@(OozeAndFilth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      agendas <- select AnyAgenda
      chooseOrRunOneM iid $ targets agendas (attachTreachery attrs)
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> OozeAndFilth <$> liftRunMessage msg attrs
