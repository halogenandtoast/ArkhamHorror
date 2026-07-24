module Arkham.Location.Cards.ChamberOfPoison (chamberOfPoison) where

import Arkham.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher (locationIs)
import Arkham.Message.Lifted.Log
import Arkham.Name
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype ChamberOfPoison = ChamberOfPoison LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfPoison :: LocationCard ChamberOfPoison
chamberOfPoison =
  locationWith ChamberOfPoison Cards.chamberOfPoison 4 (Static 0)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 1) (locationIs Cards.chamberOfRot)

instance HasAbilities ChamberOfPoison where
  getAbilities (ChamberOfPoison a) =
    extendRevealed1 a $ onlyOnce $ restricted a 1 Here actionAbility

instance RunMessage ChamberOfPoison where
  runMessage msg l@(ChamberOfPoison attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      name <- field InvestigatorName iid
      remember $ TurnedTheValve $ labeled name iid
      pure l
    _ -> ChamberOfPoison <$> liftRunMessage msg attrs
