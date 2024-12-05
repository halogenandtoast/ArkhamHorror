module Arkham.Location.Cards.HereticsGraves_171 (hereticsGraves_171, HereticsGraves_171 (..)) where

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Message (ReplaceStrategy (..))

newtype HereticsGraves_171 = HereticsGraves_171 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hereticsGraves_171 :: LocationCard HereticsGraves_171
hereticsGraves_171 = location HereticsGraves_171 Cards.hereticsGraves_171 7 (Static 0)

instance HasModifiersFor HereticsGraves_171 where
  getModifiersFor (HereticsGraves_171 a) =
    getSkillTestInvestigator >>= \case
      Nothing -> pure mempty
      Just iid -> maybeModified_ a iid do
        liftGuardM $ isInvestigating iid a.id
        willpower <- lift $ getSkillValue #willpower iid
        pure [AnySkillValue willpower]

instance RunMessage HereticsGraves_171 where
  runMessage msg l@(HereticsGraves_171 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      spectral <- genCard Locations.hereticsGravesSpectral_171
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> HereticsGraves_171 <$> liftRunMessage msg attrs
