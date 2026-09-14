module Arkham.Location.Cards.TheDrownedCity.TheGrandVault.ChamberOfTheTabletUnsealed (chamberOfTheTabletUnsealed) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Direction (GridDirection (GridLeft))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.CardDefs.TheDrownedCity.TheGrandVault qualified as Cards
import Arkham.Location.Grid (updatePosition)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ChamberOfTheTabletUnsealed = ChamberOfTheTabletUnsealed LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTheTabletUnsealed :: LocationCard ChamberOfTheTabletUnsealed
chamberOfTheTabletUnsealed = location ChamberOfTheTabletUnsealed Cards.chamberOfTheTabletUnsealed 3 (PerPlayer 2)

instance HasModifiersFor ChamberOfTheTabletUnsealed where
  getModifiersFor (ChamberOfTheTabletUnsealed a) = do
    modifySelf a [CannotBeFlooded]
    -- Connected to the location to the left of it, and vice versa.
    for_ (locationPosition a) \pos -> do
      let leftPos = updatePosition pos GridLeft
      modifySelf a [ConnectedToWhen (be a) (LocationInPosition leftPos)]
      modifySelect a (LocationInPosition leftPos) [ConnectedToWhen (LocationInPosition leftPos) (be a)]

instance HasAbilities ChamberOfTheTabletUnsealed where
  getAbilities (ChamberOfTheTabletUnsealed a) =
    extendRevealed1 a
      $ groupLimit PerWindow
      $ restricted a 1 (exists $ SetAsideCardMatch $ cardIs Assets.tidalTablet)
      $ freeReaction
      $ DiscoveringLastClue #after Anyone (be a)

instance RunMessage ChamberOfTheTabletUnsealed where
  runMessage msg l@(ChamberOfTheTabletUnsealed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ investigatorAt attrs.id
      withSetAsideCard Assets.tidalTablet \tablet ->
        chooseOrRunOneM iid $ targets investigators (`takeControlOfSetAsideAsset` tablet)
      pure l
    _ -> ChamberOfTheTabletUnsealed <$> liftRunMessage msg attrs
