module Arkham.Treachery.Cards.PulledBack (pulledBack, PulledBack (..)) where

import Arkham.Direction
import Arkham.Helpers.Location
import Arkham.Helpers.Scenario
import Arkham.Investigator.Projection ()
import Arkham.Location.Grid
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PulledBack = PulledBack TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pulledBack :: TreacheryCard PulledBack
pulledBack = treachery PulledBack Cards.pulledBack

instance RunMessage PulledBack where
  runMessage msg t@(PulledBack attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      grid <- getGrid
      void $ runMaybeT do
        loc <- MaybeT $ getLocationOf iid
        pos <- hoistMaybe $ findInGrid loc grid
        GridLocation _ east <- hoistMaybe $ viewGrid (updatePosition pos East) grid
        lift do
          moveTo attrs iid east
          ks <- iid.keys
          for_ ks $ placeKey east
      pure t
    _ -> PulledBack <$> liftRunMessage msg attrs
