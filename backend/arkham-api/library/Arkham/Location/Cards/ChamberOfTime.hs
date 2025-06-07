module Arkham.Location.Cards.ChamberOfTime (chamberOfTime) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype ChamberOfTime = ChamberOfTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTime :: LocationCard ChamberOfTime
chamberOfTime = location ChamberOfTime Cards.chamberOfTime 4 (PerPlayer 2) & setConnectsTo (singleton RightOf)

instance HasAbilities ChamberOfTime where
  getAbilities (ChamberOfTime a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)

instance RunMessage ChamberOfTime where
  runMessage msg l@(ChamberOfTime attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      relicOfAges <- fetchCard [Assets.relicOfAgesRepossessThePast, Assets.relicOfAgesADeviceOfSomeSort]
      createAssetAt_ relicOfAges (AttachedToLocation attrs.id)
      placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> ChamberOfTime <$> liftRunMessage msg attrs
