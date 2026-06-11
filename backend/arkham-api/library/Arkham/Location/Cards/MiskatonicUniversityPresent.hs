module Arkham.Location.Cards.MiskatonicUniversityPresent (miskatonicUniversityPresent) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Scientist))

newtype MiskatonicUniversityPresent = MiskatonicUniversityPresent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityPresent :: LocationCard MiskatonicUniversityPresent
miskatonicUniversityPresent =
  location MiskatonicUniversityPresent Cards.miskatonicUniversityPresent 3 (PerPlayer 1)

instance HasAbilities MiskatonicUniversityPresent where
  getAbilities (MiskatonicUniversityPresent a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> Remembered ATreeSeedHasBeenPlanted) actionAbility

instance RunMessage MiskatonicUniversityPresent where
  runMessage msg l@(MiskatonicUniversityPresent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- selectCount $ AssetWithTrait Scientist <> AssetAt (be attrs)
      gainClues iid (attrs.ability 1) n
      pure l
    _ -> MiskatonicUniversityPresent <$> liftRunMessage msg attrs
