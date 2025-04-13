module Arkham.Location.Cards.ReturnToEngineCar (returnToEngineCar) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Direction
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype ReturnToEngineCar = ReturnToEngineCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToEngineCar :: LocationCard ReturnToEngineCar
returnToEngineCar =
  locationWith ReturnToEngineCar Cards.returnToEngineCar 3 (PerPlayer 2)
    $ connectsToL
    .~ singleton LeftOf

instance HasModifiersFor ReturnToEngineCar where
  getModifiersFor (ReturnToEngineCar a) = do
    whenUnrevealed a $ blockedWhenAny a $ leftOf a <> LocationWithAnyClues
    modifySelect a Anyone [CannotInvestigateLocation a.id]

instance HasAbilities ReturnToEngineCar where
  getAbilities (ReturnToEngineCar a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ RevealLocation #after You (be a)

instance RunMessage ReturnToEngineCar where
  runMessage msg l@(ReturnToEngineCar attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      createAssetAt_ Assets.engineer (AtLocation attrs.id)
      pure l
    _ -> ReturnToEngineCar <$> liftRunMessage msg attrs
