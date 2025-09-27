module Arkham.Location.Cards.HereticsGravesSpectral_171 (hereticsGravesSpectral_171) where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheWagesOfSin.Helpers

newtype HereticsGravesSpectral_171 = HereticsGravesSpectral_171 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGravesSpectral_171 :: LocationCard HereticsGravesSpectral_171
hereticsGravesSpectral_171 = location HereticsGravesSpectral_171 Cards.hereticsGravesSpectral_171 7 (Static 0)

instance HasModifiersFor HereticsGravesSpectral_171 where
  getModifiersFor (HereticsGravesSpectral_171 a) =
    whenJustM getSkillTestInvestigator \iid -> maybeModified_ a iid do
      liftGuardM $ isInvestigating iid a.id
      pure [AnySkillValueCalculated $ InvestigatorFieldCalculation iid #willpower]

instance HasAbilities HereticsGravesSpectral_171 where
  getAbilities (HereticsGravesSpectral_171 a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "hereticsGravesSpectral_171.haunted" a 1

instance RunMessage HereticsGravesSpectral_171 where
  runMessage msg l@(HereticsGravesSpectral_171 attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.hereticsGraves_171
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      effectWithSource (attrs.ability 1) iid do
        removeOn $ EffectNextTurnWindow iid
        apply $ BaseSkillOf #willpower 1
      pure l
    _ -> HereticsGravesSpectral_171 <$> liftRunMessage msg attrs
