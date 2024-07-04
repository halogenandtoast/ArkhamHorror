module Arkham.Treachery.Cards.SpiritsTorment (spiritsTorment, SpiritsTorment (..)) where

import Arkham.Ability
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher
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
    , restrictedAbility a 2 OnSameLocation $ actionAbilityWithCost (PlaceClueOnLocationCost 1)
    ]

instance RunMessage SpiritsTorment where
  runMessage msg t@(SpiritsTorment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasActions <- elem iid <$> select InvestigatorWithAnyActionsRemaining
      if hasActions
        then
          chooseOne
            iid
            [ Label "Take 1 horror" [Msg.assignHorror iid (attrs.ability 1) 1]
            , Label "Lose 1 action" [LoseActions iid (attrs.ability 1) 1]
            ]
        else assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> SpiritsTorment <$> liftRunMessage msg attrs
