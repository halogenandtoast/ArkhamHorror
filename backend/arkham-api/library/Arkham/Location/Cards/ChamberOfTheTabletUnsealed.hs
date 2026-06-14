module Arkham.Location.Cards.ChamberOfTheTabletUnsealed (chamberOfTheTabletUnsealed) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Direction (GridDirection (GridLeft))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid (updatePosition)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ChamberOfTheTabletUnsealed = ChamberOfTheTabletUnsealed LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTheTabletUnsealed :: LocationCard ChamberOfTheTabletUnsealed
chamberOfTheTabletUnsealed = location ChamberOfTheTabletUnsealed Cards.chamberOfTheTabletUnsealed 3 (Static 2)

instance HasModifiersFor ChamberOfTheTabletUnsealed where
  getModifiersFor (ChamberOfTheTabletUnsealed a) = do
    modifySelf a [CannotBeFlooded]
    -- Connected to the location to the left of it, and vice versa.
    case locationPosition a of
      Nothing -> pure ()
      Just pos -> do
        let leftPos = updatePosition pos GridLeft
        modifySelf a [ConnectedToWhen (be a) (LocationInPosition leftPos)]
        modifySelect a (LocationInPosition leftPos) [ConnectedToWhen (LocationInPosition leftPos) (be a)]

instance HasAbilities ChamberOfTheTabletUnsealed where
  getAbilities (ChamberOfTheTabletUnsealed a) =
    extendRevealed1 a $ mkAbility a 1 $ freeReaction $ DiscoveringLastClue #after Anyone (be a)

instance RunMessage ChamberOfTheTabletUnsealed where
  runMessage msg l@(ChamberOfTheTabletUnsealed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ investigatorAt attrs.id
      tablet <- getSetAsideCard Assets.tidalTablet
      chooseOrRunOneM iid $ targets investigators (`takeControlOfSetAsideAsset` tablet)
      pure l
    _ -> ChamberOfTheTabletUnsealed <$> liftRunMessage msg attrs
