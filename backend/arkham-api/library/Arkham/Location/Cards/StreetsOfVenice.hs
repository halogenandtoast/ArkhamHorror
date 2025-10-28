module Arkham.Location.Cards.StreetsOfVenice (streetsOfVenice) where

import Arkham.Ability
import Arkham.Direction
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (MoveAction)
import Arkham.Message qualified as Msg

newtype StreetsOfVenice = StreetsOfVenice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetsOfVenice :: LocationCard StreetsOfVenice
streetsOfVenice =
  locationWith StreetsOfVenice Cards.streetsOfVenice 2 (Static 2)
    $ connectsToL
    .~ singleton RightOf

instance HasAbilities StreetsOfVenice where
  getAbilities (StreetsOfVenice attrs) =
    extendRevealed1 attrs $ restricted attrs 1 Here $ FastAbility Free

instance RunMessage StreetsOfVenice where
  runMessage msg l@(StreetsOfVenice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select (AccessibleFrom ForMovement $ be attrs)
      case locations of
        [] -> error "No connections?"
        (x : _) -> push $ Msg.MoveAction iid x Free False
      pure l
    _ -> StreetsOfVenice <$> liftRunMessage msg attrs
