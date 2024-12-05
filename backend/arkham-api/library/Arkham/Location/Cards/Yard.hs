module Arkham.Location.Cards.Yard (yard, Yard (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype Yard = Yard LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yard :: LocationCard Yard
yard = location Yard Cards.yard 1 (PerPlayer 1)

instance HasModifiersFor Yard where
  getModifiersFor (Yard a) = whenRevealed a $ maybeModifySelf a do
    iid <- MaybeT getSkillTestInvestigator
    liftGuardM $ isInvestigating iid a
    horror <- field InvestigatorHorror iid
    pure [ShroudModifier horror]

instance HasAbilities Yard where
  getAbilities (Yard attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (Here <> NoCluesOnThis)
      $ actionAbilityWithCost (DamageCost (toSource attrs) YouTarget 1)

instance RunMessage Yard where
  runMessage msg l@(Yard attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember IncitedAFightAmongstThePatients
      pure l
    _ -> Yard <$> liftRunMessage msg attrs
