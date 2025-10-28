module Arkham.Location.Cards.HauntedFieldsSpectral (hauntedFieldsSpectral) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Trait (Trait (Spectral))

newtype HauntedFieldsSpectral = HauntedFieldsSpectral LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hauntedFieldsSpectral :: LocationCard HauntedFieldsSpectral
hauntedFieldsSpectral = location HauntedFieldsSpectral Cards.hauntedFieldsSpectral 3 (Static 0)

instance HasModifiersFor HauntedFieldsSpectral where
  getModifiersFor (HauntedFieldsSpectral attrs) = do
    modifySelect attrs (enemyAt (toId attrs) <> EnemyWithTrait Spectral) [HorrorDealt 1]

instance HasAbilities HauntedFieldsSpectral where
  getAbilities (HauntedFieldsSpectral a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "hauntedFieldsSpectral.haunted" a 1

instance RunMessage HauntedFieldsSpectral where
  runMessage msg l@(HauntedFieldsSpectral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ NearestEnemyTo iid $ EnemyWithTrait Spectral
      chooseTargetM iid enemies \enemy -> moveToward enemy (LocationWithId attrs.id)
      pure l
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.hauntedFields
      pure l
    _ -> HauntedFieldsSpectral <$> liftRunMessage msg attrs
