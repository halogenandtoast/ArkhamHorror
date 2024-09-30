module Arkham.Location.Cards.ChurningWaters (churningWaters, ChurningWaters (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChurningWaters = ChurningWaters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

churningWaters :: LocationCard ChurningWaters
churningWaters = location ChurningWaters Cards.churningWaters 6 (Static 0)

instance HasAbilities ChurningWaters where
  getAbilities (ChurningWaters a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> youExist (InVehicleMatching $ assetIs Assets.fishingVessel))
      $ ActionAbility [#resign] (ActionCost 1)

instance RunMessage ChurningWaters where
  runMessage msg l@(ChurningWaters attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      act <- selectJust AnyAct
      selectEach (InVehicleMatching $ assetIs Assets.fishingVessel) \iid -> do
        resign iid
        ks <- iid.keys
        for_ ks \k -> when (k `elem` [PurpleKey, WhiteKey, BlackKey]) $ placeKey act k
      mFishingVessel <- selectOne $ assetIs Assets.fishingVessel
      for_ mFishingVessel removeFromGame
      pure l
    _ -> ChurningWaters <$> liftRunMessage msg attrs
