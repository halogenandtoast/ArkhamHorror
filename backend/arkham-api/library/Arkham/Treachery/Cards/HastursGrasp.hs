module Arkham.Treachery.Cards.HastursGrasp (hastursGrasp) where

import Arkham.Ability
import Arkham.Agenda.Sequence
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HastursGrasp = HastursGrasp TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hastursGrasp :: TreacheryCard HastursGrasp
hastursGrasp = treachery HastursGrasp Cards.hastursGrasp

instance HasAbilities HastursGrasp where
  getAbilities (HastursGrasp a) =
    [ restricted a 1 InYourHand
        $ forced
        $ PlacedDoomCounter #after AnySource
        $ AgendaTargetMatches
        $ AgendaWithSide A
    ]

instance RunMessage HastursGrasp where
  runMessage msg t@(HastursGrasp attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 2
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> HastursGrasp <$> liftRunMessage msg attrs
