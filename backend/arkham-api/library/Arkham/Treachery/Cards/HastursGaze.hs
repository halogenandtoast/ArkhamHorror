module Arkham.Treachery.Cards.HastursGaze (hastursGaze) where

import Arkham.Ability
import Arkham.Agenda.Sequence
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HastursGaze = HastursGaze TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hastursGaze :: TreacheryCard HastursGaze
hastursGaze = treachery HastursGaze Cards.hastursGaze

instance HasAbilities HastursGaze where
  getAbilities (HastursGaze a) =
    [ restricted a 1 InYourHand
        $ forced
        $ PlacedDoomCounter #after AnySource
        $ AgendaTargetMatches
        $ AgendaWithSide C
    ]

instance RunMessage HastursGaze where
  runMessage msg t@(HastursGaze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 2
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> HastursGaze <$> liftRunMessage msg attrs
