module Arkham.Location.Cards.MiskatonicUniversityMiskatonicMuseum (
  miskatonicUniversityMiskatonicMuseum,
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (
  miskatonicUniversityMiskatonicMuseum,
 )
import Arkham.Location.Import.Lifted

newtype MiskatonicUniversityMiskatonicMuseum = MiskatonicUniversityMiskatonicMuseum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityMiskatonicMuseum
  :: LocationCard MiskatonicUniversityMiskatonicMuseum
miskatonicUniversityMiskatonicMuseum =
  location
    MiskatonicUniversityMiskatonicMuseum
    Cards.miskatonicUniversityMiskatonicMuseum
    3
    (PerPlayer 1)

instance HasAbilities MiskatonicUniversityMiskatonicMuseum where
  getAbilities (MiskatonicUniversityMiskatonicMuseum a) =
    extendRevealed1 a $ playerLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage MiskatonicUniversityMiskatonicMuseum where
  runMessage msg l@(MiskatonicUniversityMiskatonicMuseum attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 2
      gainClues iid (attrs.ability 1) 1
      pure l
    _ -> MiskatonicUniversityMiskatonicMuseum <$> liftRunMessage msg attrs
