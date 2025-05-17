module Arkham.Treachery.Cards.SpiritsTorment (spiritsTorment) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SpiritsTorment = SpiritsTorment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritsTorment :: TreacheryCard SpiritsTorment
spiritsTorment = treachery SpiritsTorment Cards.spiritsTorment

instance HasAbilities SpiritsTorment where
  getAbilities (SpiritsTorment a) =
    [ mkAbility a 1 $ forced $ Leaves #when You $ locationWithTreachery a
    , restricted a 2 OnSameLocation $ actionAbilityWithCost (PlaceClueOnLocationCost 1)
    ]

instance RunMessage SpiritsTorment where
  runMessage msg t@(SpiritsTorment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasActions <- elem iid <$> select InvestigatorWithAnyActionsRemaining
      chooseOrRunOneM iid $ withI18n do
        chooseTakeHorror iid (attrs.ability 1) 1
        when hasActions $ chooseLoseActions iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> SpiritsTorment <$> liftRunMessage msg attrs
