module Arkham.Act.Cards.MysteriousGateway (mysteriousGateway) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MysteriousGateway = MysteriousGateway ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mysteriousGateway :: ActCard MysteriousGateway
mysteriousGateway =
  act
    (1, A)
    MysteriousGateway
    Cards.mysteriousGateway
    (Just $ GroupClueCost (PerPlayer 3) "Guest Hall")

instance RunMessage MysteriousGateway where
  runMessage msg a@(MysteriousGateway attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      investigators <- select $ InvestigatorAt "Guest Hall"
      holeInTheWall <- placeSetAsideLocation Locations.holeInTheWall
      sid <- getRandom
      leadChooseOneM do
        targets investigators \iid' -> do
          moveTo attrs iid' holeInTheWall
          beginSkillTest sid iid' attrs iid' #willpower (Fixed 4)
      advanceActDeck attrs
      pure a
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      randomDiscardN iid attrs n
      pure a
    _ -> MysteriousGateway <$> liftRunMessage msg attrs
