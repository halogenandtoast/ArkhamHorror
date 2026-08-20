module Arkham.Location.Cards.TheCircleUndone.TheWagesOfSin.HauntedFields (hauntedFields) where

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.CardDefs.TheCircleUndone.TheWagesOfSin qualified as Cards
import Arkham.Location.CardDefs.TheCircleUndone.TheWagesOfSin qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Spectral))

newtype HauntedFields = HauntedFields LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hauntedFields :: LocationCard HauntedFields
hauntedFields = location HauntedFields Cards.hauntedFields 3 (PerPlayer 2)

instance HasModifiersFor HauntedFields where
  getModifiersFor (HauntedFields attrs) = do
    modifySelect attrs (enemyAt (toId attrs) <> EnemyWithTrait Spectral) [HorrorDealt 1]

instance RunMessage HauntedFields where
  runMessage msg l@(HauntedFields attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.hauntedFieldsSpectral
      pure l
    _ -> HauntedFields <$> liftRunMessage msg attrs
