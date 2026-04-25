module Arkham.Location.Cards.MiskatonicUniversityInFlames (miskatonicUniversityInFlames) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (miskatonicUniversityInFlames)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MiskatonicUniversityInFlames = MiskatonicUniversityInFlames LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityInFlames :: LocationCard MiskatonicUniversityInFlames
miskatonicUniversityInFlames = location MiskatonicUniversityInFlames Cards.miskatonicUniversityInFlames 4 (PerPlayer 1)

instance HasAbilities MiskatonicUniversityInFlames where
  getAbilities (MiskatonicUniversityInFlames a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ investigatorAt a)
      $ forced
      $ PhaseEnds #when #investigation

instance RunMessage MiskatonicUniversityInFlames where
  runMessage msg l@(MiskatonicUniversityInFlames attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select (investigatorAt attrs.id)
      assets <- select (AssetAt (LocationWithId attrs.id) <> AssetWithHealth)
      for_ investigators \iid -> directDamage iid (attrs.ability 1) 1
      for_ assets \asset -> dealAssetDamage asset (attrs.ability 1) 1
      pure l
    _ -> MiskatonicUniversityInFlames <$> liftRunMessage msg attrs
