module Arkham.Act.Cards.ToTheTower (toTheTower) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Trait (Trait (Tower))

newtype ToTheTower = ToTheTower ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toTheTower :: ActCard ToTheTower
toTheTower = act (2, A) ToTheTower Cards.toTheTower Nothing

instance HasModifiersFor ToTheTower where
  getModifiersFor (ToTheTower a) = do
    modifySelect a (LocationWithTrait Tower) [ScenarioModifier "noCityOfRemnants"]

instance HasAbilities ToTheTower where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1 $ Objective $ forced $ Enters #after Anyone "Gravity-Defying Climb"

instance RunMessage ToTheTower where
  runMessage msg a@(ToTheTower attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ToTheTower <$> liftRunMessage msg attrs
