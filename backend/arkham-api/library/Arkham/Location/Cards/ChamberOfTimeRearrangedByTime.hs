module Arkham.Location.Cards.ChamberOfTimeRearrangedByTime (chamberOfTimeRearrangedByTime) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype ChamberOfTimeRearrangedByTime = ChamberOfTimeRearrangedByTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTimeRearrangedByTime :: LocationCard ChamberOfTimeRearrangedByTime
chamberOfTimeRearrangedByTime =
  location ChamberOfTimeRearrangedByTime Cards.chamberOfTimeRearrangedByTime 4 (PerPlayer 2)
    & setLabel "chamberOfTime"
    & setConnectsTo (singleton RightOf)

instance HasAbilities ChamberOfTimeRearrangedByTime where
  getAbilities (ChamberOfTimeRearrangedByTime a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)

instance RunMessage ChamberOfTimeRearrangedByTime where
  runMessage msg l@(ChamberOfTimeRearrangedByTime attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      relicOfAges <- fetchCard [Assets.relicOfAgesRepossessThePast, Assets.relicOfAgesADeviceOfSomeSort]
      createAssetAt_ relicOfAges (AttachedToLocation attrs.id)
      placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> ChamberOfTimeRearrangedByTime <$> liftRunMessage msg attrs
