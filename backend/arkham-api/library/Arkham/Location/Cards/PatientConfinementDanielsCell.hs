module Arkham.Location.Cards.PatientConfinementDanielsCell (patientConfinementDanielsCell) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PatientConfinementDanielsCell = PatientConfinementDanielsCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementDanielsCell :: LocationCard PatientConfinementDanielsCell
patientConfinementDanielsCell =
  locationWith
    PatientConfinementDanielsCell
    Cards.patientConfinementDanielsCell
    2
    (Static 1)
    (costToEnterUnrevealedL .~ ClueCost (Static 1))

instance HasAbilities PatientConfinementDanielsCell where
  getAbilities (PatientConfinementDanielsCell a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage PatientConfinementDanielsCell where
  runMessage msg l@(PatientConfinementDanielsCell attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceCurrentAct attrs
      pure l
    _ -> PatientConfinementDanielsCell <$> liftRunMessage msg attrs
