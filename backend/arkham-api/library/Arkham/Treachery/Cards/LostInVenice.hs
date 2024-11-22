module Arkham.Treachery.Cards.LostInVenice (lostInVenice, LostInVenice (..)) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LostInVenice = LostInVenice TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInVenice :: TreacheryCard LostInVenice
lostInVenice = treachery LostInVenice Cards.lostInVenice

instance RunMessage LostInVenice where
  runMessage msg t@(LostInVenice attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      field InvestigatorLocation iid >>= \case
        Nothing -> assignDamage iid attrs 2
        Just lid ->
          getAcrossLocation lid >>= \case
            Nothing -> assignDamage iid attrs 2
            Just acrossLocationId -> do
              chooseOneM iid do
                labeled "Take 2 horror" $ assignHorror iid attrs 2
                labeled "Move to the location across from you" $ moveTo attrs iid acrossLocationId
      pure t
    _ -> LostInVenice <$> liftRunMessage msg attrs
