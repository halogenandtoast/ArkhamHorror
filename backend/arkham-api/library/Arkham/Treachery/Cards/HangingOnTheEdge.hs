module Arkham.Treachery.Cards.HangingOnTheEdge (hangingOnTheEdge) where

import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Expedition))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HangingOnTheEdge = HangingOnTheEdge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangingOnTheEdge :: TreacheryCard HangingOnTheEdge
hangingOnTheEdge = treachery HangingOnTheEdge Cards.hangingOnTheEdge

instance RunMessage HangingOnTheEdge where
  runMessage msg t@(HangingOnTheEdge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- genId
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      assets <- select $ assetControlledBy iid <> withTrait Expedition
      mloc <- getMaybeLocation iid
      let handleAsset = maybe (toDiscard attrs) (flip place) mloc
      chooseOneAtATimeM iid $ targets assets handleAsset

      for_ mloc \loc -> do
        row <- maybe 0 (.row) <$> field LocationPosition loc
        moveTo_ attrs iid $ LocationInRow (row - 1)

      pure t
    _ -> HangingOnTheEdge <$> liftRunMessage msg attrs
