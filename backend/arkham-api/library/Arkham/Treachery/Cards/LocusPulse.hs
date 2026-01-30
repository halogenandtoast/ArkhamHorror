module Arkham.Treachery.Cards.LocusPulse (locusPulse) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Scenarios.DogsOfWar.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LocusPulse = LocusPulse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

locusPulse :: TreacheryCard LocusPulse
locusPulse = treachery LocusPulse Cards.locusPulse

instance HasAbilities LocusPulse where
  getAbilities (LocusPulse a) =
    [ restricted a 1 (thisExists a $ TreacheryAttachedToLocation $ not_ locationWithKeyLocus)
        $ forced AnyWindow
    ]

instance RunMessage LocusPulse where
  runMessage msg t@(LocusPulse attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <- select $ NearestLocationTo iid locationWithKeyLocus
      chooseOneToHandle iid attrs ls
      pure t
    HandleTargetChoice _iid (isSource attrs -> True) (LocationTarget lid) -> do
      attachTreachery attrs lid
      selectEach (investigatorAt lid) \iid' -> assignDamageAndHorror iid' attrs 1 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> LocusPulse <$> liftRunMessage msg attrs
