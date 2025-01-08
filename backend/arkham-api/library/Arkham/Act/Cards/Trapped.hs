module Arkham.Act.Cards.Trapped (trapped) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query (enemiesAt, getJustLocationByName)
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Move

newtype Trapped = Trapped ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trapped :: ActCard Trapped
trapped = act (1, A) Trapped Cards.trapped (groupClueCost $ PerPlayer 2)

instance RunMessage Trapped where
  runMessage msg a@(Trapped attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      study <- getJustLocationByName "Study"
      enemies <- enemiesAt study

      hallway <- placeSetAsideLocation Locations.hallway
      placeSetAsideLocation_ Locations.cellar
      placeSetAsideLocation_ Locations.attic
      placeSetAsideLocation_ Locations.parlor

      for_ enemies (toDiscard attrs)
      reveal hallway
      moveAllTo attrs hallway
      removeLocation study
      advanceActDeck attrs
      pure a
    _ -> Trapped <$> liftRunMessage msg attrs
